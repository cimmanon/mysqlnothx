{-# LANGUAGE OverloadedStrings #-}

module Parser.Data where

import Data.Monoid ((<>), mconcat, mappend)
import Control.Applicative

import Prelude hiding (takeWhile)
import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (mapMaybe)
import Data.List (find, intersperse)
import Data.Char (intToDigit, ord, chr, readLitChar)
import Data.Word (Word8)
import Numeric (showIntAtBase)

import Data.Construct
import Parser.Common
import Parser.Construct (constructIdentifier)

{----------------------------------------------------------------------------------------------------{
																	  | Inserts
}----------------------------------------------------------------------------------------------------}

-- INSERT INTO "indexes" VALUES (0,'foo','bar'),(1,'foo','bar'),(2,'baz','buz');
insertToCopy :: StatefulParser BS.ByteString
insertToCopy = do
	tableName <- lift (string "INSERT INTO" *> spaces *> constructIdentifier)
	tableList <- constructs <$> get
	currentTable <- find (isNamedTable tableName) <$> constructs <$> get
	let
		columns = columnTypes . body <$> currentTable

--		valList :: Parser [BS.ByteString]
		valList = maybe unknownTypes knownTypes columns <?> "valList"

--		valGroup :: Parser BS.ByteString
		valGroup = BS.intercalate "\t" <$> (string "(" *> valList <* string ")") <?> "values"
	vals <- lift (spaces *> string "VALUES" *> spaces *> csv valGroup <?> "list of values")
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
		getTableName (Table n _) = Just n
		getTableName _ = Nothing
		quoteIdentifier (ConstructIdentifier xs) = "\"" <> BS.intercalate "." xs <> "\""

{-
Knowing exactly which types we're expecting lets us correctly translate quoted
values to their appropriate type.  MySQL generates some really bad stuff for
date, timestamp, bit, and blob types in dump files.  This lets us compensate
for it.
-}
knownTypes :: [Scalar] -> Parser [BS.ByteString]
knownTypes (x : xs) =
	sequence $ scalarParser' x : (map (\s -> comma *> scalarParser' s) xs)
	where
		scalarParser' s = sqlNull <|> scalarParser s
knownTypes _ = return []

{-
If we don't know our types, we fall back on inference and just parse a list of
known token formats.
-}
unknownTypes :: Parser [BS.ByteString]
unknownTypes = csv value
	where
		value =
				sqlNull
			<|> hexToCopy
			<|> numericToken
			<|> zeroDateTime
			<|> mysqlQuotedString
			<|> binaryToken'
			<?> "unknown types"
		binaryToken' = (string "b'" <|> string "B'") *> takeWhile (`elem8` ['0', '1']) <* string "'"
		-- ^^ TODO: fix code duplication

scalarParser :: Scalar -> Parser BS.ByteString
scalarParser (Text _) = mysqlQuotedString
scalarParser (Char _) = mysqlQuotedString
scalarParser (Varchar _) = mysqlQuotedString
scalarParser (Enum _) = mysqlQuotedString
scalarParser (Set _) = toArrayFormat <$> enum
scalarParser (Bit i) = hexToBinary i <|> unquote (charToBinary i)
scalarParser (Blob _) = hexToCopy <|> unquote blobToHex
scalarParser (Numeric _ _) = numericToken
scalarParser (Float _) = numericToken
scalarParser (Double _) = numericToken
scalarParser (Integer _ _) = numericToken
scalarParser (Date) = date
scalarParser (Time) = mysqlQuotedString
scalarParser (DateTime) = dateTime
scalarParser (Timestamp) = timestamp
scalarParser (Date) = mysqlQuotedString
scalarParser (Unknown _) = mysqlQuotedString

sqlNull :: Parser BS.ByteString
sqlNull = string "NULL" >> return "\\N"

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

{----------------------------------------------------------------------------------------------------{
                                                                      | Dates / Times
}----------------------------------------------------------------------------------------------------}

zeroDateTime :: Parser BS.ByteString
zeroDateTime = (string "'0000-00-00 00:00:00'" <|> string "'0000-00-00'") >> return "-infinity"

date :: Parser BS.ByteString
date = zeroDateTime <|> mysqlQuotedString

dateTime :: Parser BS.ByteString
dateTime = zeroDateTime <|> mysqlQuotedString

timestamp :: Parser BS.ByteString
timestamp = zeroDateTime <|> mysqlQuotedString

{----------------------------------------------------------------------------------------------------{
                                                                      | Binary (Bit / Blob)
}----------------------------------------------------------------------------------------------------}

-- convert an Integer to a padded Binary
intBin :: (Integral a, Show a) => Int -> a -> BS.ByteString
intBin precision = padStr . word8ToBinary
	where
		word8ToBinary w = showIntAtBase 2 intToDigit w ""
		padding i = BS.concat $ Prelude.take i $ repeat "0"
		padStr x = padding (precision - length x) <> BSC.pack x

-- This should handle all "characters" that have been escaped and should be
-- converted to their actual Char value (\n, \r, \\, \0)
improperlyEscapedChar :: Parser Char
improperlyEscapedChar = ungraceful <|> graceful
	where
		ungraceful = string "\\Z" *> pure (chr 26)
		graceful = do
			c <- BS.concat <$> sequence [ string "\\", Data.Attoparsec.ByteString.take 1 ]
			case readLitChar (BSC.unpack c) of
				(x : _) -> return $ fst x
				_ -> fail $ "readLitChar failed to parse " <> BSC.unpack c

---------------------------------------------------------------------- | Default format

{-
When MySQL dumps a binary value, it will come out in one of two formats:
	"\\0" -> that's a backslash followed by a character rather than '\0' for 0
	"<"   -> a single ASCII character starting from '\1'

When we get the 2nd type of format, we have to get it's character number, then
convert it to a binary representation.  In order to import it into PostgreSQL,
it has to be padded with zeros so that the final length is the same as the
precision for the column.
-}
charToBinary :: Int -> Parser BS.ByteString
charToBinary precision = leadingSlash <|> rest
	where
		leadingSlash = toBin . ord <$> improperlyEscapedChar
		rest = toBin <$> anyWord8
		toBin x = intBin precision x

blobToHex :: Parser BS.ByteString
blobToHex = (mappend "\\\\x") <$> mconcat <$> many1 (chars <|> specialChar)
	where
		word8ToHex w =
			let
				h = showIntAtBase 16 intToDigit w ""
				padding = Prelude.take (2 - length h) "00"
			in padding  <> h
		chars = bsToHex <$> takeWhile1 (\x -> not $ x `elem8` ['\\', '\''])
		specialChar = BSC.pack . word8ToHex . ord <$> improperlyEscapedChar
		bsToHex = BSC.pack . concat . map word8ToHex . BS.unpack

---------------------------------------------------------------------- | With --hex-blob flag

{-
If you dump using the `--hex-blob` flag, it converts all BIT and BLOB values to
unquoted hexadecimal values.
-}
hex :: Parser BS.ByteString
hex =
	mconcat <$> many1 (numbers <|> letters)
	where
		numbers = takeWhile1 (`elem8` ['0' .. '9'])
		letters = takeWhile1 (`elem8` "abcdefABCDEF")

hexToCopy :: Parser BS.ByteString
hexToCopy =
	(string "0x" *> return "\\\\x") <++> option "" hex

hexToBinary :: Int -> Parser BS.ByteString
hexToBinary precision = intBin precision <$> hexInt
	where
		hexInt :: Parser Int
		hexInt = read . BSC.unpack . BS.concat <$> sequence [string "0x", hex]

{----------------------------------------------------------------------------------------------------{
                                                                      | Set / Enum
}----------------------------------------------------------------------------------------------------}

{-
PostgreSQL Array output:
{one}
{two,three}
-}
toArrayFormat :: [BS.ByteString] -> BS.ByteString
toArrayFormat xs = BS.concat [ "{\"", BS.intercalate "\",\"" xs, "\"}" ]

enum :: Parser [BS.ByteString]
enum = unquote (csv insideQuotes)
	where
		insideQuotes = BS.concat <$> many1 (specialChars <|> noSpecialChars) <?> "inside quotes"
		noSpecialChars = takeWhile1 (\ x -> not $ x `elem8` ['\\', '\'', ',']) <?> "no special chars"
		-- TODO: fix the specialChars parser
		specialChars =
				(string "\"" >> return "\\\"") -- escape double quotes
			<|> (string "\\\"" >> return "\"") -- unescape double quotes
			<|> (string "\\\\" >> return "\\\\") -- preserve escaping on backslashes
			-- TODO: double check if this is optimal
			<|> string "\\" -- otherwise just return the slash
