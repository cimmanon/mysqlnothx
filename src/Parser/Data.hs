{-# LANGUAGE OverloadedStrings #-}

module Parser.Data where

import Data.Monoid ((<>), mconcat)
import Control.Applicative

import Prelude hiding (concat, takeWhile)
import Control.Monad.Trans.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (find)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

import Data.Construct
import Parser.Common
import Parser.Construct (constructIdentifier)

{----------------------------------------------------------------------------------------------------{
																	  | Inserts
}----------------------------------------------------------------------------------------------------}

-- INSERT INTO "indexes" VALUES (0,'foo','bar'),(1,'foo','bar'),(2,'baz','buz');
insertToCopy :: Parser BS.ByteString
insertToCopy = do
	tableName <- string "INSERT INTO" *> spaces *> (quoteIdentifier <$> constructIdentifier)
	vals <- spaces *> string "VALUES" *> spaces *> unknownTypes <?> "list of values"
	endOfStatement
	skipSpace
	return $ BS.concat
		[ "COPY "
		, tableName
		, " FROM stdin;\n"
		, BS.intercalate "\n" vals
		, "\n\\.\n"
		]
	where
		unknownTypes = csv values
		values = BS.intercalate "\t" <$> (string "(" *> csv value <* string ")") <?> "values"
		value =
				sqlNull
			<|> hexToCopy
			<|> numericToken
			<|> zeroDateTime
			<|> mysqlQuotedString
			<|> binaryToken'
			<?> "value"
		binaryToken' = (string "b'" <|> string "B'") *> takeWhile (`elem8` ['0', '1']) <* string "'"
		-- ^^ TODO: fix code duplication
		sqlNull = string "NULL" >> return "\\N"
		zeroDateTime = (string "'0000-00-00 00:00:00'" <|> string "'0000-00-00'") >> return "-infinity"
		quoteIdentifier (ConstructIdentifier xs) = "\"" <> BS.intercalate "." xs <> "\""

insertToCopy' :: StatefulParser BS.ByteString
insertToCopy' = do
	tableName <- lift (string "INSERT INTO" *> spaces *> constructIdentifier)
	currentTable <- find (isNamedTable tableName) <$> constructs <$> get
	let
		columns = (filter isColumn . body) <$> currentTable
	vals <- lift (spaces *> string "VALUES" *> spaces *> unknownTypes <?> "list of values")
	lift endOfStatement
	lift skipSpace
	return $ BS.concat
		[ "COPY "
		, quoteIdentifier tableName
		, " FROM stdin;\n"
		, BS.intercalate "\n" vals
		, "\n\\.\n"
		]
	where
		isNamedTable x (Table n _) = n == x
		isNamedTable _ _ = False

		unknownTypes = csv values
		values = BS.intercalate "\t" <$> (string "(" *> csv value <* string ")") <?> "values"
		value =
				sqlNull
			<|> hexToCopy
			<|> numericToken
			<|> zeroDateTime
			<|> mysqlQuotedString
			<|> binaryToken'
			<?> "value"
		binaryToken' = (string "b'" <|> string "B'") *> takeWhile (`elem8` ['0', '1']) <* string "'"
		-- ^^ TODO: fix code duplication
		sqlNull = string "NULL" >> return "\\N"
		zeroDateTime = (string "'0000-00-00 00:00:00'" <|> string "'0000-00-00'") >> return "-infinity"
		quoteIdentifier (ConstructIdentifier xs) = "\"" <> BS.intercalate "." xs <> "\""

-- insert scathing rant here about how MySQL switches its method of escaping single quotes for inserts
mysqlQuotedString :: Parser BS.ByteString
mysqlQuotedString = quote *> (emptyString <|> (insideQuotes <* quote))
	where
		insideQuotes = mconcat <$> many1 (specialChars <|> noSpecialChars) <?> "inside quotes"
		quote = string "'"
		emptyString = quote >> return ""
		noSpecialChars = takeWhile1 (\ x -> not $ x `elem8` ['\\', '\'', '\t']) <?> "no special chars"
		specialChars =
				(string "\t" >> return "\\t") -- escape tabs
			<|> (string "\\0" >> return "\\\\0") -- double escape "null" characters
			-- ^^ when using COPY, outputting the string `\\0` will be treated as a `\0` when the query is run
			<|> (string "\\'" >> return "'") -- unescape single quotes
			<|> (string "\\\"" >> return "\"") -- unescape double quotes
			<|> (string "\\\\" >> return "\\\\") -- preserve escaping on backslashes
			-- TODO: double check if this is optimal
			<|> string "\\" -- otherwise just return the slash

hexToCopy :: Parser BS.ByteString
hexToCopy =
	(string "0x" *> return "\\\\x") <++> option "" (mconcat <$> many1 (numbers <|> letters))
	where
		numbers = takeWhile1 (`elem8` ['0' .. '9'])
		letters = takeWhile1 (`elem8` "abcdefABCDEF")

{-
When MySQL dumps a binary value, it will come out in one of two formats:
	"\\0" -> that's a backslash followed by a zero rather than '\0' for 0
	"<"   -> a single ASCII character starting from '\1'

When we get the 2nd type of format, we have to get it's character number, then
convert it to a binary representation.  In order to import it into PostgreSQL,
it has to be padded with zeros so that the final length is the same as the
precision for the column.
-}
charToBinary :: Int -> Parser BS.ByteString
charToBinary precision = zero <|> rest
	where
		zero = string "\\0" *> pure (padding precision)
		rest = do
			binary <- word8ToBinary <$> anyWord8
			return $ padding (precision - length binary) <> BSC.pack binary
		padding i = BS.concat $ Prelude.take i $ repeat "0"
		word8ToBinary w = showIntAtBase 2 intToDigit w ""
