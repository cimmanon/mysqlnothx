{-# LANGUAGE OverloadedStrings #-}

module Parser.Construct where

import Data.Tuple (swap)
import Control.Applicative

import Prelude hiding (concat, takeWhile)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.Char (toUpper)

import Data.Construct
import Parser.Common

{----------------------------------------------------------------------------------------------------{
																	  | Indexes
}----------------------------------------------------------------------------------------------------}

-- MySQL (reversed order)
-- CREATE INDEX index_name ON table (col1, col2) USING BTREE;
-- don't think we'll actually see this in a dump, since MySQL tends to put it in the table definition

-- PostgreSQL (standard order)
-- CREATE INDEX index_name ON table USING index_type (col1, col2 desc);
-- CREATE UNIQUE INDEX index_name_key ON table USING index_type (col1, col2) WHERE (col3 = true);
-- ^ partial unique index

createIndex :: Parser Construct
createIndex = do
	indexName <- string "CREATE INDEX" *> spaces *> optionalToken
	tableName <- skipSpace *> string "ON" *> spaces *> constructIdentifier
	(iType, columnInfo) <- (swap <$> reversedOrder) <|> standardOrder
	skipSpace *> endOfStatement
	return (Table tableName [Index indexName columnInfo iType])
	where
		standardOrder = ( , )
			<$> optionalIndexType
			<*> indexColumns
		reversedOrder = ( , )
			<$> indexColumns
			<*> optionalIndexType

-- MySQL (part of table creation), note that INDEX and KEY are interchangeable
-- INDEX optional_index_name (list of columns/functions + sort)
-- INDEX `amount` (`amount`) USING BTREE,

mysqlIndex :: Parser TableAttribute
mysqlIndex = Index
	<$> do (string "INDEX" <|> string "KEY") *> spaces *> optionalToken
	<*> do skipSpace *> indexColumns
	<*> maybeMatch (spaces *> indexType <* skipSpace)

indexColumns :: Parser [(Identifier, Maybe BS.ByteString)]
indexColumns = skipSpace *> string "(" *> manyCols <* string ")"
	where
		manyCols = csv (skipSpace *> col <* skipSpace)
		col = ( , )
			<$> (mysqlIndexColumn <|> expression)
			<*> maybeMatch (spaces *> oneToken)

indexType :: Parser BS.ByteString
indexType = string "USING" *> spaces *> unquotedToken

optionalIndexType :: Parser (Maybe BS.ByteString)
optionalIndexType = maybeMatch (spaces *> indexType)

-- MySQL has a limit on how many characters you can have in a unique index
-- you can specify the length by using this syntax:  KEY (column_name(200))
-- http://stackoverflow.com/a/1827118/1652962
-- NOTE: we're just discarding the index length here

mysqlIndexColumn :: Parser BS.ByteString
mysqlIndexColumn = (mysqlQuotedIdentifier <|> unquotedToken) <* option "" mysqlIndexLength

-- TODO: add this to the unique constraint parser as well
mysqlIndexLength :: Parser BS.ByteString
mysqlIndexLength = (string "(" *> takeWhile1 (`elem8` ['0'..'9']) <* string ")")

{----------------------------------------------------------------------------------------------------{
																	  | Tables
}----------------------------------------------------------------------------------------------------}

-- TODO: parse `FULLTEXT KEY `search` (`title`,`content`)` in the table body

createTable :: Parser Construct
createTable = Table <$> tableName <*> tableBody
	where
		tableName = string "CREATE TABLE" *> spaces *> constructIdentifier
		tableBody = do
			skipSpace
			string "(" *> skipSpace
			tableParts
			-- confirm/consume the rest of the table definition and discard it
			<* skipSpace
			<* string ")"
			<* skipWhile (isNot ';')
			<* endOfStatement

tableParts :: Parser [TableAttribute]
tableParts = csv (skipSpace *> bodyType <* skipSpace)
	where
		bodyType = constraint <|> mysqlIndex <|> tableColumn

-- ALTER TABLE ONLY size_systems ADD CONSTRAINT size_systems_pkey PRIMARY KEY (name);
-- ALTER TABLE               baz ADD CONSTRAINT baz_bar FOREIGN KEY (bar) REFERENCES bar(name);

alterTable :: Parser Construct
alterTable = Table
	<$> do string "ALTER TABLE" *> spaces *> optionalOnly *> constructIdentifier <* spaces
	<*> do (: []) <$> (string "ADD" *> spaces *> constraint <* skipSpace <* endOfStatement <* skipSpace)
	where
		-- the ONLY keyword most likely won't have any special meaning for us
		-- since we're designing around parsing dumps
		-- psql uses it to indicate that only the specified table and not the
		-- descendant tables will be altered, and appears to always be generated
		-- by pg_dump (so we might always want to generate it on output to psql?)
		optionalOnly = (string "ONLY" *> spaces *> return ()) <|> skipSpace

{----------------------------------------------------------------------------------------------------{
																	  | Shared
}----------------------------------------------------------------------------------------------------}

constructIdentifier :: Parser ConstructIdentifier
constructIdentifier = ConstructIdentifier <$> oneIdentifier `sepBy` string "."

---------------------------------------------------------------------- | Constraints

-- CONSTRAINT optional_constraint_name FOREIGN KEY (list of columns) REFERENCES table_name (list of columns)

-- MySQL
-- UNIQUE KEY optional_constraint_name ([Identifier]) USING index_type

-- PostgreSQL
-- UNIQUE optional_constraint_name ([Identifier])
-- CONSTRAINT optional_constraint_name CHECK (expression)

constraint :: Parser TableAttribute
constraint = skipSpace *> mysqlUniqueConstraint <|> standardConstraint
	where
		standardConstraint = Constraint
			<$> maybeMatch (string "CONSTRAINT" *> spaces *> oneToken <* spaces)
			<*> do primaryKey <|> foreignKey <|> unique <|> check
		primaryKey = PrimaryKey
			<$> do string "PRIMARY KEY" *> spaces *> listOfColumns <* optionalIndexType <* skipSpace
			-- ^^ NOTE: we're discarding the index type information here
		foreignKey = ForeignKey
			<$> do string "FOREIGN KEY" *> spaces *> listOfColumns
			<*> do skipSpace *> string "REFERENCES" *> spaces *> reference <* skipSpace
			<*> maybeMatch (string "ON DELETE" *> spaces *> cascadeType <* skipSpace)
			<*> maybeMatch (string "ON UPDATE" *> spaces *> cascadeType <* skipSpace)
		unique = Unique
			<$> do string "UNIQUE" *> spaces *> listOfColumns <* skipSpace
		check = Check <$> do string "CHECK" *> spaces *> expression <* skipSpace
		mysqlUniqueConstraint = Constraint
			<$> do skipSpace *> string "UNIQUE" *> maybeMatch (spaces *> string "KEY") *> spaces *> maybeMatch (oneToken <* spaces)
			<*> do Unique <$> listOfColumns <* optionalIndexType <* skipSpace
			-- ^^ NOTE: we're discarding the index type information here
		reference = ( , )
			<$> do skipSpace *> constructIdentifier
			<*> do skipSpace *> tokenListGroup
		cascadeType =
				string "SET NULL"
			<|> string "SET DEFAULT"
			<|> string "RESTRICT"
			<|> string "CASCADE"
			<|> string "NO ACTION"
		listOfColumns = string "(" *> csv (mysqlIndexColumn <|> oneToken) <* string ")"

---------------------------------------------------------------------- | Columns

{-
Identifier Type [OPTIONS]

OPTIONS:
	NOT NULL | NULL
	PRIMARY KEY (won't appear here in dumps)
	UNIQUE (won't appear here in dumps)
	DEFAULT expression
	COMMENT quoted_string (MySQL)
	AUTO_INCREMENT (MySQL)
	unsigned (MySQL -- numeric type columns)
	CHARACTER SET charset_name (MySQL -- text type columns)
	COLLATE collation_name (MySQL, PostgreSQL 9.1+)
	ON UPDATE expression (MySQL)
-}

tableColumn :: Parser TableAttribute
tableColumn =
	Column
		<$> do skipSpace *> oneToken <* spaces
		<*> columnType <* skipSpace -- everything after the column type is optional
		<*> (options `sepBy` spaces) <* skipSpace
	where
		options =
				(string "NOT NULL" >> return (Nullable False))
			<|> (string "NULL" >> return (Nullable True))
			<|> do Default <$> (string "DEFAULT" *> spaces *> expression)
			<|> do InlineComment <$> (string "COMMENT" *> spaces *> sqlQuotedString)
			<|> (string "AUTO_INCREMENT" >> return AutoIncrement)
			<|> (string "PRIMARY KEY" >> return InlinePrimaryKey)
			<|> (string "UNIQUE" >> return InlineUnique)
			<|> do CharSet <$> (string "CHARACTER SET" *> spaces *> oneToken)
			<|> do Collate <$> (string "COLLATE" *> spaces *> oneToken)
			<|> do OnUpdate <$> (string "ON UPDATE" *> spaces *> expression)

columnType :: Parser Scalar
columnType = knownColumnType <|> fmap Unknown unknownColumnType

knownColumnType :: Parser Scalar
knownColumnType = do
	x <- unquotedToken
	case map toUpper (BSC.unpack x) of
		-- Text
		"TINYTEXT" -> return $ Text 1
		"TEXT" -> return $ Text 2
		"MEDIUMTEXT" -> return $ Text 3
		"LONGTEXT" -> return $ Text 4
		"VARCHAR" -> Varchar <$> simplePrecision
		"CHAR" -> Char <$> simplePrecision

		-- Integer
		"TINYINT" -> Integer 1 <$> intAttributes
		"SMALLINT" -> Integer 2 <$> intAttributes
		"MEDIUMINT" -> Integer 3 <$> intAttributes
		"INT" -> Integer 4 <$> intAttributes
		"INTEGER" -> Integer 4 <$> intAttributes
		"BIGINT" -> Integer 8 <$> intAttributes

		-- Numeric
		"FLOAT" -> Float <$> (maybeMatch precisePrecision *> signed)
		"DOUBLE" -> Double <$> (maybeMatch precisePrecision *> signed)
		"NUMERIC" -> Numeric <$> precisePrecision <*> signed

		-- Date
		"DATE" -> return Date
		"TIME" -> return Time
		"DATETIME" -> return DateTime
		"TIMESTAMP" -> return Timestamp

		-- Binary
		"TINYBLOB" -> return $ Blob 1
		"BLOB" -> return $ Blob 2
		"MEDIUMBLOB" -> return $ Blob 3
		"LONGBLOB" -> return $ Blob 4
		"BIT" -> Bit <$> simplePrecision

		_ -> fail "Unknown column type"
	where
		-- the precision value is purely for formatting/display purposes, so ignore it
		intAttributes = (maybeMatch simplePrecision) *> signed
		signed = (spaces *> string "unsigned" *> pure False) <|> pure True
		simplePrecision = string "(" *> int <* string ")"
		precisePrecision = do
			string "("
			x <- int
			comma
			y <- int
			string ")"
			return (x, y)

unknownColumnType :: Parser BS.ByteString
unknownColumnType = complex <|> simple
	where
		-- matches types with parentheses: VARCHAR(10), ENUM('foo', 'bar')
		complex = unquotedToken <++> string "(" <++> takeTill (is ')') <++> string ")"
		-- TODO: add support for nested arrays
		--array = unquotedToken <++> string "[" <++> takeTill (is ']') <++> string "]"
		-- matches simple types:  INT, TEXT, INET
		simple = unquotedToken
