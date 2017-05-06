{-# LANGUAGE OverloadedStrings #-}

module Output.PostgreSQL where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List (partition, find)
import Data.Maybe (isJust, mapMaybe)
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

{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

quoteIdentifier :: Identifier -> BS.ByteString
quoteIdentifier = O.quoteIdentifier '"'

quoteString :: BS.ByteString -> BS.ByteString
quoteString = O.quoteString '\'' '\''

constructIdentifierToText :: ConstructIdentifier -> BS.ByteString
constructIdentifierToText (ConstructIdentifier xs) = BS.intercalate "." $ map quoteIdentifier xs

{----------------------------------------------------------------------------------------------------{
                                                                      | Constructs
}----------------------------------------------------------------------------------------------------}

constructToText :: Construct -> BS.ByteString
constructToText x@(Table {}) = tableToText x
constructToText x@(Comment {}) = commentToText x
constructToText x@(Sequence {}) = O.doNotOutput x

firstPass :: [Construct] -> BS.ByteString
firstPass xs =
	O.sectionComment "Tables"
	<> BS.intercalate "\n" (O.removeEmpties $ map constructToText $ transformConstructs xs)

secondPass :: [Construct] -> BS.ByteString
secondPass xs' =
	BS.concat
		[ O.sectionComment "Unique Constraints"
		, BS.intercalate "\n" (O.removeEmpties $ map constraintToText notFk)
		, "\n"
		, O.sectionComment "Indexes"
		, BS.intercalate "\n" (O.removeEmpties $ map indexToText indexes)
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
		, BS.intercalate "\n" (O.removeEmpties $ map constraintToText exists)
		, "\n-- Constraints referencing tables that aren't part of the dump file.\n\n"
		, "SAVEPOINT imaginary_constraints;\n\n"
		, BS.intercalate "\n" (O.removeEmpties $ map constraintToText notExists) <> "\n\n"
		]
	where
		xs = transformConstructs xs'
		justTables = filter isTable xs
		sequences = filter isSequence xs
		(columns, notColumns) = O.partitionTables isColumn justTables
		(indexes, constraints) = O.partitionTables isIndex notColumns
		(fk, notFk) = O.partitionConstraints constraints
		(exists, notExists) = O.partitionTables (O.isRealConstraint notFk) fk

{----------------------------------------------------------------------------------------------------{
                                                                      | Transformers
}----------------------------------------------------------------------------------------------------}

transformConstructs :: [Construct] -> [Construct]
transformConstructs = concatMap transformConstruct

transformConstruct :: Construct -> [Construct]
transformConstruct (Table n xs) =
	let
		(columns, notColumns) = partition isColumn xs
		(attrs, constructs) = unzip $ map (transformColumn n) columns
	in
		Table n (notColumns ++ concat attrs) : concat constructs
transformConstruct _ = []

transformColumn :: ConstructIdentifier -> TableAttribute -> ([TableAttribute], [Construct])
transformColumn n@(ConstructIdentifier ix) (Column n' t xs) =
	let
		(constructs, xs') = partition columnAttributeIsConstruct xs
		(tableAttrs, columnAttrs) = partition columnAttributeIsTableAttribute xs'
	in
		(Column n' t columnAttrs : map toTableAttribute tableAttrs, map toConstruct constructs)
	where
		toTableAttribute x = case x of
			InlinePrimaryKey -> Constraint Nothing (PrimaryKey [n'])
			InlineUnique -> Constraint Nothing (Unique [n'])
		toConstruct x = case x of
			AutoIncrement -> Sequence (BS.intercalate "_" ix <> "_" <> n' <> "_seq") 1 1 (Just $ appendIdentifier n n')
			InlineComment c -> Comment "COLUMN" (appendIdentifier n n') c

columnAttributeIsConstruct :: ColumnAttribute -> Bool
columnAttributeIsConstruct x = case x of
	AutoIncrement -> True
	InlineComment _ -> True
	_ -> False

columnAttributeIsTableAttribute :: ColumnAttribute -> Bool
columnAttributeIsTableAttribute x = case x of
	InlinePrimaryKey -> True
	InlineUnique -> True
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

{-
CREATE TABLE (
	[columns]
);
-}

tableToText :: Construct -> BS.ByteString
tableToText (Table n b) | not (null justColumns) =
	BS.concat
		[ "CREATE TABLE "
		, constructIdentifierToText n
		, " (\n\t"
		, flattenColumns
		, "\n);\n"
		]
	where
		justColumns = filter isColumn b
		flattenColumns = BS.intercalate ",\n\t" $ map columnToText justColumns

{----------------------------------------------------------------------------------------------------{
                                                                      | Table Columns
}----------------------------------------------------------------------------------------------------}

columnToText :: TableAttribute -> BS.ByteString
columnToText (Column n t attrs) = format $ quoteIdentifier n : definition
	where
		isAuto = isJust $ find isAutoIncrement attrs
		intToBS = C.pack . show
		simplePrecision n i = BS.concat [ n, "(", intToBS $ max 1 i, ")"]
		-- ^ MySQL allows non-standard precision of 0 for CHAR/VARCHAR, we have to guarantee that it's >= 1
		definition = case t of
			Text _ -> "TEXT" : textAttributes
			Char i -> simplePrecision "CHAR" i : textAttributes
			Varchar i ->  simplePrecision "VARCHAR" i : textAttributes

			Bit i -> simplePrecision "BIT" i : defaultAttributes
			Blob _ -> "BYTEA" : defaultAttributes

			Numeric (x, y) signed -> BS.concat [ "NUMERIC(", intToBS x, ",", intToBS y, ")" ] : defaultAttributes
			Float signed -> "REAL" : defaultAttributes
			Double signed -> "REAL" : defaultAttributes
			-- integer types require going up to the next biggest type when it's unsigned
			-- PostgreSQL doesn't have a TINYINT (Integer 1) or MEDIUMINT (Integer 3)
			Integer i True | isAuto && i <= 3 -> "SMALLSERIAL" : defaultAttributes
			Integer i _    | isAuto -> "SERIAL" : defaultAttributes
			Integer i True | i <= 3 -> "SMALLINT" : defaultAttributes
			Integer 3 False         -> "INTEGER" : defaultAttributes
			Integer i True | i <= 4 -> "INTEGER" : defaultAttributes
			Integer _ _             -> "BIGINT" : defaultAttributes

			Date -> "DATE" : dateAttributes
			Time -> "TIME" : timestampAttributes
			DateTime -> "TIMESTAMP" : timestampAttributes
			Timestamp -> "TIMESTAMPTZ" : timestampAttributes
			-- ^ TODO: evaluate the correctness of using TIMESTAMPTZ for a MySQL TIMESTAMP

			Unknown x -> x : defaultAttributes

		format = BS.intercalate " "

		defaultAttributes = mapMaybe defaultAttributeToText attrs
		textAttributes = mapMaybe textAttributeToText attrs
		dateAttributes = mapMaybe dateAttributeToText attrs
		timestampAttributes = mapMaybe timestampAttributeToText attrs
columnToText _ = ""

---------------------------------------------------------------------- | Attributes for specific column types

-- TODO: deal with default values appropriately

defaultAttributeToText :: ColumnAttribute -> Maybe BS.ByteString
defaultAttributeToText (Nullable False) = Just "NOT NULL"
defaultAttributeToText _ = Nothing

-- text
textAttributeToText :: ColumnAttribute -> Maybe BS.ByteString
textAttributeToText (Default "''") = Nothing -- Don't allow empty strings by default
textAttributeToText (Default expr) = Just $ "DEFAULT " <> expr
textAttributeToText (Collate x) = Nothing -- don't output this until we can translate it properly
--textAttributeToText (Collate x) = Just $ "COLLATE " <> x
textAttributeToText x = defaultAttributeToText x

-- dates
dateAttributeToText :: ColumnAttribute -> Maybe BS.ByteString
dateAttributeToText (Default "0000-00-00") = Just "DEFAULT CURRENT_TIMESTAMP"
dateAttributeToText (Default expr) = Just $ "DEFAULT " <> expr
dateAttributeToText (OnUpdate _) = Nothing -- TODO: do something with this
dateAttributeToText x = defaultAttributeToText x

-- timestamps
timestampAttributeToText :: ColumnAttribute -> Maybe BS.ByteString
timestampAttributeToText (Default "0000-00-00 00:00:00") = Just "DEFAULT CURRENT_TIMESTAMP"
timestampAttributeToText (OnUpdate _) = Nothing -- TODO: do something with this
timestampAttributeToText x = dateAttributeToText x

-- enum/set
-- TODO

{----------------------------------------------------------------------------------------------------{
                                                                      | Comments
}----------------------------------------------------------------------------------------------------}

{-
COMMENT ON [construct type] [construct identifier] IS [comment];
-}
commentToText :: Construct -> BS.ByteString
commentToText (Comment t n c) =
	BS.concat
		[ "COMMENT ON "
		, t
		, " "
		, constructIdentifierToText n
		, " IS "
		, quoteString c
		,  ";\n"
		]

{-
{-
COMMENT ON ConstructType ConstructIdentifier IS Text

COMMENT ON COLUMN my_table.my_column IS 'Employee ID number';
COMMENT ON TABLE my_schema.my_table IS 'Employee Information';
COMMENT ON FUNCTION my_function (timestamp) IS 'Returns Roman Numeral';
COMMENT ON CONSTRAINT bar_col_cons ON bar IS 'Constrains column col';
-}

comment :: BS.ByteString -> ConstructIdentifier -> BS.ByteString -> BS.ByteString
--comment objType objName c = "COMMENT ON " <> objType <> " " <> (quoteIdentifier defaultParams) objName <> " " <>  (quoteString defaultParams) c <> ";"
comment objType objName c =
	BS.concat
		[ "/* COMMENT ON "
		, objType
		, " "
		, constructIdentifierToText objName
		, " "
		, quoteString c
		, "; */"
		]
-}
{----------------------------------------------------------------------------------------------------{
                                                                      | Sequences
}----------------------------------------------------------------------------------------------------}

{-
SELECT setval(pg_get_serial_sequence('[table name]', '[column name]'), 1), (SELECT COALESCE(max([column name]), 1) FROM [table name]));
-}
resetSequence :: Construct -> BS.ByteString
resetSequence (Sequence n s i (Just (ConstructIdentifier o))) =
	-- not sure why the table name needs to be quoted (for case sensitivity purposes)
	-- when passed to pg_get_serial_sequence, but the column name should not,
	-- but that's the rules.  still need to quote it when referencing it elsewhere
	let
		table = BS.intercalate "." $ map quoteIdentifier $ init o
		column = last o
	in
		BS.concat
			[ "SELECT setval(pg_get_serial_sequence('"
			, table
			, "', '"
			, column
			, "'), (SELECT COALESCE(max("
			, quoteIdentifier column
			, "), 1) FROM "
			, table
			, "));"
			]
resetSequence _ = ""

{-
CREATE SEQUENCE [identifier] START [] INCREMENT [increment amount];
ALTER SEQUENCE [identifier] OWNED BY [table identifier];
ALTER TABLE [] ALTER COLUMN [table identifier] SET DEFAULT NEXTVAL('[current count]');
-}
sequenceToText :: Construct -> BS.ByteString
sequenceToText (Sequence n s i o) =
	BS.concat
		[ "CREATE SEQUENCE "
		, quoteIdentifier n
		, " START "
		, toText s
		, " INCREMENT "
		, toText i
		, ";\n"
	--	, maybe "" setOwner o
		]
	where
		toText = C.pack . show
		{-
		-- TODO: fix setting ownership
		setOwner (ConstructIdentifier i) =
			BS.concat
				[ "ALTER SEQUENCE "
				, quoteIdentifier n
				, " OWNED BY "
				, constructIdentifierToText ownerIdentifier
				, ";\n"
				, "ALTER TABLE "
				, constructIdentifierToText (ConstructIdentifier $ init i)
				, " ALTER COLUMN "
				, quoteIdentifier $ last i
				, " SET DEFAULT NEXTVAL('"
				, n
				, "');\n"
		-}

{----------------------------------------------------------------------------------------------------{
																	  | Inserts
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Indexes
}----------------------------------------------------------------------------------------------------}

{-
CREATE INDEX [index name] ON [table name] USING [index type] ([column identifiers]);
-}
indexToText :: Construct -> BS.ByteString
indexToText (Table name body) =
	BS.concat $ map toText $ filter isIndex body
	where
		toText (Index n cs x) =
			BS.concat
				[ "CREATE INDEX "
				, maybe "" quoteIdentifier n
				, " ON "
				, constructIdentifierToText name
				, maybe "" (" USING " <> ) x
				, " ("
				, BS.intercalate ", " (map colToText cs)
				, ");\n"
				]
		-- TODO: quoting was removed on `n` because it is now an expression.
		-- We need to add it back in where appropriate, but that requires
		-- significant change to the parser.
		colToText (n, x) = quoteIdentifier n <> maybe "" (" " <> ) x

{----------------------------------------------------------------------------------------------------{
                                                                      | Constraints
}----------------------------------------------------------------------------------------------------}

--------------------------------------------------------------------- | Constraint

{-
ALTER TABLE [table identifier] ADD [constraint information];
-}
constraintToText :: Construct -> BS.ByteString
constraintToText (Table name body) =
	BS.concat $ map toText justColumns
	where
		justColumns = filter isConstraint body
		toText (Constraint name' info) =
			BS.concat
			[ "ALTER TABLE "
			, constructIdentifierToText name
			, " ADD "
			, maybe "" (\x -> BS.concat ["CONSTRAINT ", x, " "]) name'
			, theRest info
			, ";\n"
			]
		theRest t =
			case t of
				-- TODO: quote the identifiers here
				PrimaryKey cols -> "PRIMARY KEY (" <> toCSV' cols <> ")"
				Unique cols -> "UNIQUE (" <> toCSV' cols <> ")"
				ForeignKey cols (ref, refCols) onDelete onUpdate ->
					"FOREIGN KEY (" <> toCSV' cols
					<> ") REFERENCES " <> constructIdentifierToText ref <> " (" <> toCSV' refCols <> ")"
				_ -> "Not implemented"
		toCSV' = O.toCSV . (map quoteIdentifier)
