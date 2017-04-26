{-# LANGUAGE OverloadedStrings #-}

module Output.PostgreSQL where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List (partition, find)
import Data.Maybe (isJust)
import Data.Monoid ((<>))

import Data.Construct
import qualified Output as O

{-
MySQLisms to PostgreSQL
	AUTO_INCREMENT = create a sequence
	UNSIGNED = create a check constraint > 0, but only if it isn't foreign keyed
	CHARSET = ignore
	COLLATE = ignore
	ON UPDATE = create a trigger
	timestamp NOT NULL DEFAULT '0000-00-00 00:00:00' = timestamptz DEFAULT CURRENT_TIMESTAMP (see: http://gusiev.com/2009/04/update-and-create-timestamps-with-mysql/)

For the ON UPDATE MySQLism, we want to create a trigger like this:

CREATE OR REPLACE FUNCTION set_${column_name}_to_now()
RETURNS TRIGGER AS $$
BEGIN
	NEW.${column_name} = now();
	RETURN NEW;
END;
$$ language 'plpgsql';


CREATE TRIGGER ${table_name}_${column_name}_to_now BEFORE UPDATE
	ON ${table_name} FOR EACH ROW EXECUTE PROCEDURE set_${column_name}_to_now();
-}

constructToText :: Construct -> BS.ByteString
constructToText x@(Table {}) = O.tableToText x
constructToText x@(Comment {}) = O.commentToText x
constructToText x@(Sequence {}) = O.doNotOutput x

firstPass :: [Construct] -> BS.ByteString
firstPass xs =
	O.sectionComment "Tables"
	<> BS.intercalate "\n" (O.removeEmpties $ map constructToText xs)

secondPass :: [Construct] -> BS.ByteString
secondPass xs =
	BS.concat
		[ O.sectionComment "Unique Constraints"
		, BS.intercalate "\n" (O.removeEmpties $ map O.constraintToText notFk)
		, "\n"
		, O.sectionComment "Indexes"
		, BS.intercalate "\n" (O.removeEmpties $ map O.indexToText indexes)
		, "\n"
		, O.sectionComment "Reset Sequences"
		, BS.intercalate "\n" (O.removeEmpties $ map resetSequence sequences)
		, "\n"
		, O.sectionComment "Foreign Keys"
		{-
		MySQL allows tables to exist that have foreign keys that reference non-existant tables
		when created with `SET FOREIGN_KEY_CHECKS = 0;`

		For now, we're going to split up the foreign keys into a existing/not existing.  The
		"existing" foreign keys will be generated, then the not existing foreign keys will be
		generated inside comments.  Thinking about having this be configurable, since it might
		be nice to automatically run the "not existing" foreign keys, but outside of the
		transaction.
		-}
		, BS.intercalate "\n" (O.removeEmpties $ map O.constraintToText exists)
		, "\n-- Constraints referencing tables that aren't part of the dump file.\n\n"
		, "SAVEPOINT imaginary_constraints;\n\n"
		, BS.intercalate "\n" (O.removeEmpties $ map O.constraintToText notExists) <> "\n\n"
		]
	where
		justTables = filter isTable xs
		sequences = filter isSequence xs
		(columns, notColumns) = O.partitionTables isColumn justTables
		(indexes, constraints) = O.partitionTables isIndex notColumns
		(fk, notFk) = O.partitionConstraints constraints
		(exists, notExists) = O.partitionTables (isRealConstraint notFk) fk

{----------------------------------------------------------------------------------------------------{
																	  | Transformers
}----------------------------------------------------------------------------------------------------}

transformConstructs :: [Construct] -> [Construct]
transformConstructs = concatMap transformConstruct

transformConstruct :: Construct -> [Construct]
transformConstruct (Table n xs) =
	let
		(columns, notColumns) = partition isColumn xs
		(attrs, constructs) = unzip $ map (transformColumn n . columnFromMysql) columns
	in
		Table n (notColumns ++ concat attrs) : concat constructs

transformColumn :: ConstructIdentifier -> TableAttribute -> ([TableAttribute], [Construct])
transformColumn n@(ConstructIdentifier ix) (Column n' t xs) =
	let
		(constructs, xs') = partition columnAttributeIsConstruct xs
		(tableAttrs, columnAttrs) = partition columnAttributeIsTableAttribute xs'
	in
		(Column n' t columnAttrs : map toTableAttribute tableAttrs, map toConstruct constructs)
	where
		toTableAttribute x = case x of
			PrimaryKey' -> Constraint Nothing (PrimaryKey [n'])
			Unique' -> Constraint Nothing (Unique [n'])
		toConstruct x = case x of
			AutoIncrement -> Sequence (BS.intercalate "_" ix <> "_" <> n' <> "_seq") 1 1 (Just $ appendIdentifier n n')
			Comment' c -> Comment "COLUMN" (appendIdentifier n n') c

columnAttributeIsConstruct :: ColumnAttribute -> Bool
columnAttributeIsConstruct x = case x of
	AutoIncrement -> True
	Comment' _ -> True
	_ -> False

columnAttributeIsTableAttribute :: ColumnAttribute -> Bool
columnAttributeIsTableAttribute x = case x of
	PrimaryKey' -> True
	Unique' -> True
	_ -> False

{----------------------------------------------------------------------------------------------------{
																	  | Types
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Functions
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Tables
}----------------------------------------------------------------------------------------------------}

columnFromMysql :: TableAttribute -> TableAttribute
columnFromMysql c@(Column n t xs)
--    | lcT == "tinyint(1)" && Unsigned `notElem` xs = intToBool c
	-- ^^ make sure only signed tinyints get converted to boolean
	| lcT `startsWith` "tinyint" = fromMysqlInteger $ changeType "TINYINT"
	| lcT `startsWith` "smallint" = fromMysqlInteger $ changeType "SMALLINT"
	| lcT `startsWith` "mediumint" = fromMysqlInteger $ changeType "MEDIUMINT"
	| lcT `startsWith` "int" = fromMysqlInteger $ changeType "INTEGER"
	| lcT `startsWith` "bigint" = fromMysqlInteger $ changeType "BIGINT"
	| lcT == "double" = changeType "REAL"
	| lcT == "float" = changeType "REAL"
	| lcT `startsWith` "float(" = changeType ("NUMERIC" <> BS.drop 5 t)
	| lcT `startsWith` "double(" = changeType ("NUMERIC" <> BS.drop 6 t)
	-- ^^ NOTE: not sure if this is safe, might be better off just converting it to REAL
	| lcT == "tinytext" = changeType "TEXT"
	| lcT == "mediumtext" = changeType "TEXT"
	| lcT == "longtext" = changeType "TEXT"
	| lcT `endsWith` "blob" = changeType "BYTEA"
	| lcT `startsWith` "enum(" = changeType "TEXT"
	-- ^^ TODO: handle ENUM properly
	| lcT `startsWith` "set(" = changeType "TEXT"
	-- ^^ TODO: handle SET properly (see: http://www.rdeeson.com/weblog/88/enums-user-preferences-and-the-mysql-set-datatype.html)
	| lcT == "datetime" || lcT == "timestamp"  = fromMysqlDateTime $ changeType "TIMESTAMP"
	| lcT == "date" = fromMysqlDateTime $ changeType "DATE"
	| lcT == "char(0)" || lcT == "varchar(0)" = changeType "CHAR(1)"
	-- ^^ [VAR]CHAR(0) is not allowed according to the SQL standard, but MySQL allows it anyway
	-- TODO: pick a better type (some people use it as a boolean type, but our columns are set to NOT NULL)
	| otherwise = c
	where
		lcT = C.map toLower t
		startsWith = (flip BS.isPrefixOf)
		endsWith = (flip BS.isSuffixOf)
		--recurse x y = columnFromMysql $ Column n x y
		(d, rest) = O.extractFromList isDefault xs
		changeType newType = Column n newType xs

intToBool :: TableAttribute -> TableAttribute
intToBool (Column n t xs) =
	Column n "BOOL" $ maybe xs (: rest) (changeDefault <$> d)

	where
		(d, rest) = O.extractFromList isDefault xs

		changeDefault (Default "null") = Default "null"
		changeDefault (Default "0") = Default "false"
		changeDefault _ = Default "true"

-- if our types are unsigned, then we need to bump them up to the next larger integer type.
-- MySQL's unsigned TINYINT/MEDIUMINT types are smaller than a signed SMALLINT/INTEGER, so we
-- don't need to bump those, and a BIGINT can't get any bigger
fromMysqlInteger :: TableAttribute -> TableAttribute
fromMysqlInteger c@(Column n t xs)
	| t == "TINYINT" = changeType $ if isAuto then "SMALLSERIAL" else "SMALLINT"
	| t == "MEDIUMINT" = changeType $ if isAuto then "SERIAL" else "INTEGER"
	| not isAuto && t == "SMALLINT" = changeType $ if unsigned then "INTEGER" else "SMALLINT"
	| not isAuto && t == "INTEGER" = changeType $ if unsigned then "BIGINT" else "INTEGER"
	| isAuto && t == "SMALLINT" = changeType $ if unsigned then "SERIAL" else "SMALLSERIAL"
	| isAuto && t == "INTEGER" = changeType $ if unsigned then "BIGSERIAL" else "SERIAL"
	| isAuto && t == "BIGINT" = changeType "BIGSERIAL"
	| otherwise = c
	where
		isAuto = isJust $ find isAutoIncrement xs
		unsigned = isJust $ find isUnsigned xs
		changeType newType = Column n newType xs

fromMysqlDateTime (Column n t xs) =
	Column n t $ maybe xs (: rest) (changeDefault <$> d)
	where
		(d, rest) = O.extractFromList isDefault xs
		changeDefault (Default "'0000-00-00 00:00:00'") = Default "CURRENT_TIMESTAMP"
		changeDefault (Default "'0000-00-00'") = Default "CURRENT_TIMESTAMP"
		changeDefault x = x

{-
SELECT setval(pg_get_serial_sequence('[table name]', '[column name]'), 1), (SELECT COALESCE(max([column name]), 1) FROM [table name]));
-}
resetSequence :: Construct -> BS.ByteString
resetSequence (Sequence n s i (Just (ConstructIdentifier o))) =
	-- not sure why the table name needs to be quoted (for case sensitivity purposes)
	-- when passed to pg_get_serial_sequence, but the column name should not,
	-- but that's the rules.  still need to quote it when referencing it elsewhere
	let
		table = BS.intercalate "." $ map (O.quoteIdentifier O.defaultParams) $ init o
		column = last o
	in
		BS.concat
			[ "SELECT setval(pg_get_serial_sequence('"
			, table
			, "', '"
			, column
			, "'), (SELECT COALESCE(max("
			, (O.quoteIdentifier O.defaultParams) column
			, "), 1) FROM "
			, table
			, "));"
			]
resetSequence (Sequence {}) = ""

{----------------------------------------------------------------------------------------------------{
																	  | Inserts
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Indexes, Constraints, etc.
}----------------------------------------------------------------------------------------------------}

{-
There are 2 instances where a constraint would be considered imaginary:

* The table doesn't exist
* The table exists, but the columns don't have a unique constraint on them

Both are valid in MySQL, but invalid in PostgreSQL
-}

isRealConstraint :: [Construct] -> TableAttribute -> Bool
isRealConstraint xs (Constraint _ (ForeignKey _ (t, cols) _ _)) =
	let
		isUniqueConstraint (Constraint _ (Unique cs)) = cs == cols
		isUniqueConstraint (Constraint _ (PrimaryKey cs)) = cs == cols
		isUniqueConstraint _ = False
	in
		isJust $ find (\ x -> t == name x && not (null $ filter isUniqueConstraint $ body x)) xs
