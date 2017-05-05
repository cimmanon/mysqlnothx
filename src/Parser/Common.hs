{-# LANGUAGE OverloadedStrings #-}

module Parser.Common where

import Data.Word (Word8)
import Data.Monoid ((<>), mappend, Monoid)
import Control.Applicative

import Prelude hiding (concat, takeWhile)
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString.Internal (isSpaceWord8, c2w)
import qualified Data.ByteString.Char8 as BSC (unpack)

import Data.Construct

type StatefulParser a = StateT ParserState Parser a

-- shortcut operator for mappending the results of parsers
(<++>) :: (Monoid a) => Parser a -> Parser a -> Parser a
x <++> y = mappend <$> x <*> y

is :: Char -> Word8 -> Bool
is x y = (c2w x) == y

isNot :: Char -> Word8 -> Bool
isNot x y = not $ is x y

{----------------------------------------------------------------------------------------------------{
																	  | Common
}----------------------------------------------------------------------------------------------------}

char :: Word8 -> Parser BS.ByteString
char = string . BS.singleton

isEndOfLine :: Word8 -> Bool
isEndOfLine x = x `elem8` ['\r', '\n']

endOfLine :: Parser ()
endOfLine = (string "\r\n" <|> string "\n") >> return ()

endOfStatement :: Parser BS.ByteString
endOfStatement = skipSpace *> string ";" <* skipSpace

skipSpace :: Parser ()
skipSpace = skipWhile isSpaceWord8

-- ensure that there is at least one space, then consume all of the following spaces
spaces :: Parser BS.ByteString
spaces = takeWhile1 isSpaceWord8 >> return " "

comma :: Parser BS.ByteString
comma = skipSpace *> string "," <* skipSpace

quoted :: Parser BS.ByteString -> Parser BS.ByteString
quoted p = BS.concat <$> sequence [string "'", p, string "'"]

unquote :: Parser a -> Parser a
unquote p = string "'" *> p <* string "'"

parenthesed :: Parser a -> Parser a
parenthesed p = string "(" *> skipSpace *> p <* skipSpace <* string ")"

maybeMatch :: Parser a -> Parser (Maybe a)
maybeMatch p = (Just <$> p) <|> return Nothing

csv :: Parser a -> Parser [a]
csv x = x `sepBy1` comma <?> "comma delimited list"

---------------------------------------------------------------------- | Tokens

int :: Parser Int
int = do
	x <- takeWhile1 (`elem8` "0123456789")
	return $ read $ BSC.unpack x

elem8 :: Word8 -> [Char] -> Bool
elem8 x xs = elem x $ map c2w xs

isValidUnquotedCharacter :: Word8 -> Bool
isValidUnquotedCharacter = (`elem8` ('_' : ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

numericToken :: Parser BS.ByteString
numericToken = (option "" (string "-")) <++> numbers <++> precision
	where
		numbers = takeWhile1 (`elem8` ['0'..'9'])
		precision = option "" (string "." <++> numbers)

unquotedToken :: Parser BS.ByteString
unquotedToken = numericToken <|> takeWhile1 isValidUnquotedCharacter

binaryToken :: Parser BS.ByteString
binaryToken = (\ x -> "b'" <> x <> "'") <$> do
	string "b'" <|> string "B'"
	takeWhile (`elem8` "01") <* string "\'"

oneToken :: Parser BS.ByteString
oneToken =
	sqlQuotedString <|> sqlQuotedIdentifier <|> mysqlQuotedIdentifier <|> unquotedToken

oneIdentifier :: Parser Identifier
oneIdentifier = sqlQuotedIdentifier <|> mysqlQuotedIdentifier <|> unquotedToken

optionalToken :: Parser (Maybe BS.ByteString)
optionalToken = maybeMatch oneToken

listOfTokens :: Parser [BS.ByteString]
listOfTokens = csv (skipSpace *> oneToken <* skipSpace)

tokenListGroup :: Parser [BS.ByteString]
tokenListGroup = string "(" *> listOfTokens <* string ")"

{----------------------------------------------------------------------------------------------------{
																	  | Expressions
}----------------------------------------------------------------------------------------------------}

{-
What is an expression?

value
	'foo'
	32
	'0000-00-00 00:00:00'
constant
	CURRENT_TIMESTAMP
type casting
	'2014-01-01 00:00:00' :: datetime
function invocation
	now()
comparison
	x > y
	(x > y && (x < z || y > z))
concatenation
	'some string ' || now() || ' another string'
a composition of expressions
	masklen(ip_address) = 32 or masklen(ip_address) = 128
-}

-- TODO: add support for casting
-- TODO: figure out what to do with quoted identifiers
expression :: Parser BS.ByteString
expression = skipSpace *> (complexExpression <|> simpleExpression)
	where
		simpleExpression = precidence <|> function <|> oneToken'
		complexExpression = BS.intercalate " " <$> sequence
			[ simpleExpression <* spaces
			, operator <* spaces
			, expression
			]
		function = BS.concat <$> sequence [ unquotedToken, string "(", (BS.intercalate ", " <$> expression `sepBy` comma), string ")" ]
		oneToken' = binaryToken <|> unquotedToken <|> sqlQuotedString' <|> mysqlQuotedIdentifier <|> sqlQuotedIdentifier

operator :: Parser BS.ByteString
operator = specialCharacters <|> stringOperators
	where
		specialCharacters = takeWhile1 (`elem8` "&><!=~*+-/%^|/@#")
		-- TODO: create a more comprehensive list of keyword operators
		stringOperators =
				string "OR"
			<|> string "AND"
			<|> string "ANY" <|> string "SOME" <|> string "ALL" -- array operators
			<|> negation <++> spaces <++> negatableOperators
			<|> negatableOperators
		negation = string "NOT"
		negatableOperators =
				string "IS"
			<|> string "LIKE"
			<|> string "ILIKE"
			<|> string "REGEXP" -- MySQL
			<|> string "IN"

precidence :: Parser BS.ByteString
precidence = string "(" <++> expression <++> string ")"

{----------------------------------------------------------------------------------------------------{
																	  | Quoting
}----------------------------------------------------------------------------------------------------}

-- Quoted Strings

-- MySQL
-- Uses single quote characters to contain text, and supports both backslash and single quote as the
-- escape character.  When dumped, MySQL will always use a single quote as the escape character
-- eg. 'This is a string, it\'s a good string'
-- eg. 'This is a string, it''s a good string' <- this format will be dumped

-- PostgreSQL
-- Uses single quote characters to contain text with a single quote as the escape character
-- eg. 'This is a string, it''s a good string'

-- will double escaping be a concern?
-- `select 'foo\'';` returns `foo'` in MySQL, but it is an unclosed string in PostgreSQL

-- this function correctly parses out a quoted string, stripping out the quotes
quotedString :: Char -> Char -> Parser BS.ByteString
quotedString quoteChar escapeChar = char qChar *> insideQuotes <* char qChar
	where
		eChar = c2w escapeChar
		qChar = c2w quoteChar
		insideQuotes = BS.append
			<$> takeTill (`elem` [qChar, eChar])
			<*> do BS.concat <$> many' (BS.cons <$> escapedQuote <*> insideQuotes)
		escapedQuote = string (BS.pack [eChar, qChar]) *> return qChar

sqlQuotedString :: Parser BS.ByteString
sqlQuotedString = quotedString '\'' '\''

sqlQuotedIdentifier :: Parser BS.ByteString
sqlQuotedIdentifier = quotedString '"' '\\'

mysqlQuotedIdentifier :: Parser BS.ByteString
mysqlQuotedIdentifier = quotedString '`' '\\'

-- this function correctly parses a quoted string, but doesn't strip out the quotes
sqlQuotedString' :: Parser BS.ByteString
sqlQuotedString' = string "'" <++> insideQuotes
	where
		insideQuotes = (<>)
			<$> takeTill (is '\'')
			<*> do (string "''" <++> insideQuotes) <|> string "'"

{----------------------------------------------------------------------------------------------------{
																	  | Comments
}----------------------------------------------------------------------------------------------------}

comment :: Parser BS.ByteString
comment = inlineComment <|> blockComment
	where
		inlineComment = string "--" *> takeWhile (not . isEndOfLine) <* (endOfLine <|> endOfInput) <* skipSpace
		blockComment = string "/*" *> insideComment <* skipSpace
		insideComment = (<>)
			<$> takeTill (is '*')
			<*> do string "*/" <|> (string "*" <++> insideComment)
