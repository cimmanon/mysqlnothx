{-# LANGUAGE OverloadedStrings #-}

module Output where

--import Control.Monad.Reader
import Control.Applicative ((<$>))
import Data.List (find, partition)
import Data.Maybe (fromMaybe, isJust, fromJust, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS hiding (pack, cons, snoc, singleton, replicate)
import qualified Data.ByteString.Char8 as BS (pack, cons, snoc, singleton, replicate)
import qualified Data.ByteString.Search as BS (replace)
import qualified Data.ByteString.Lazy as BS (toStrict)

import Data.Construct

{-
General flow

1. Create Types
2. Create Functions
3. Create Tables
4. Perform inserts
5. Add indexes, unique constraints (primary key, unique, check), and triggers
6. Add foreign key constraints

#5 & #6 needs to come at the end so that we have a faster import
FK constraints need to be done after the unique constraints are in place
-}

data Params = Params
	{ quoteIdentifier :: BS.ByteString -> BS.ByteString
	, quoteString :: BS.ByteString -> BS.ByteString
	, createTableColumnAttributes :: ColumnAttribute -> Bool
	, columnAttributetoText :: ColumnAttribute -> BS.ByteString
	}

defaultParams :: Params
defaultParams = Params
	{ quoteIdentifier = quoteIdentifier' '"'
	, quoteString = quoteString' '\'' '\''
	, createTableColumnAttributes = defaultCreateTableColumnAttributes
	, columnAttributetoText = defaultColumnAttributetoText
	}

{-
type Printer = Reader Params

foo :: Params -> BS.ByteString -> BS.ByteString
foo p x = (quoteIdentifier p) x

foo' :: BS.ByteString -> Printer BS.ByteString
foo' x = do
	p <- ask
	return $ (quoteIdentifier p) x

foo'' :: BS.ByteString -> Printer BS.ByteString
foo'' x = ask >>= (\ c -> return $ (quoteIdentifier c) x)
-}

----------------------------------------------------------------------

sectionComment :: BS.ByteString -> BS.ByteString
sectionComment x = divider <> "\n-- " <> x <> "\n" <> divider <> "\n\n"
	where
		divider = BS.replicate 40 '-'

{----------------------------------------------------------------------------------------------------{
																	  | Constructs
}----------------------------------------------------------------------------------------------------}

{-
NOTE: These functions are partial functions on purpose.  This allows us
to compose them together as necessary for DB specific output
-}

doNotOutput :: Construct -> BS.ByteString
doNotOutput _ = ""

--------------------------------------------------------------------- | Table

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

--------------------------------------------------------------------- | Comment

{-
COMMENT ON [construct type] IS [comment];
-}
commentToText :: Construct -> BS.ByteString
commentToText (Comment t n c) =
	BS.concat
		[ "COMMENT ON "
		, t
		, " "
		, constructIdentifierToText n
		, " IS "
		, (quoteString defaultParams) c
		,  ";\n"
		]

--------------------------------------------------------------------- | Sequence

-- TODO: move this to the Postgresql module

{-
CREATE SEQUENCE [identifier] START [] INCREMENT [increment amount];
ALTER SEQUENCE [identifier] OWNED BY [table identifier];
ALTER TABLE [] ALTER COLUMN [table identifier] SET DEFAULT NEXTVAL('[current count]');
-}
sequenceToText :: Construct -> BS.ByteString
sequenceToText (Sequence n s i o) =
	"CREATE SEQUENCE " <> (quoteIdentifier defaultParams) n
	<> " START " <> (toText s) <> " INCREMENT " <> toText i <> ";\n"
	<> maybe "" (\ ownerIdentifier ->
		let
			(ConstructIdentifier ident) = ownerIdentifier
		in
			"ALTER SEQUENCE " <> (quoteIdentifier defaultParams) n
			<> " OWNED BY " <> constructIdentifierToText ownerIdentifier <> ";\n"
			<> "ALTER TABLE " <> constructIdentifierToText (ConstructIdentifier $ init ident)
			<> " ALTER COLUMN " <> ((quoteIdentifier defaultParams) (last ident)) <> " SET DEFAULT NEXTVAL('"
			<> n <> "');\n"
		) o
	where
		toText = BS.pack . show

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
		toCSV' = toCSV . (map (quoteIdentifier defaultParams))

--------------------------------------------------------------------- | Indexes

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
				, maybe "" (quoteIdentifier defaultParams) n
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
		colToText (n, x) = (quoteIdentifier defaultParams) n <> maybe "" (" " <> ) x

{----------------------------------------------------------------------------------------------------{
																	  | Types
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Functions
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Tables
}----------------------------------------------------------------------------------------------------}

--------------------------------------------------------------------- | Columns

columnToText :: TableAttribute -> BS.ByteString
columnToText (Column n t attrs) =
	BS.intercalate " " [ (quoteIdentifier defaultParams) n, t, columnAttributesToText attrs ]

columnAttributesToText :: [ColumnAttribute] -> BS.ByteString
columnAttributesToText =
	BS.intercalate " " . map (columnAttributetoText defaultParams) . filter (createTableColumnAttributes defaultParams)

-- Determines whether or not a given ColumnAttribute is generated as part of the `CREATE TABLE` statement
defaultCreateTableColumnAttributes :: ColumnAttribute -> Bool
defaultCreateTableColumnAttributes x = case x of
	Nullable False -> True
	Default "NULL" -> False
	Default "''" -> False -- empty strings
	Default _ -> True
	Collate _ -> False
	-- ^^ TODO: reenable this after we remap the collation information between databases
	_ -> False

-- Determines how the ColumnAttribute is generated for the `CREATE TABLE` statement
defaultColumnAttributetoText :: ColumnAttribute -> BS.ByteString
defaultColumnAttributetoText x = case x of
	Nullable False -> "NOT NULL"
	Nullable True -> "NULL"
	PrimaryKey' -> "PRIMARY KEY"
	Unique' -> "UNIQUE"
	Default expr -> "DEFAULT " <> expr
	-- ^^ TODO: fix this to handle expressions correctly
	Comment' t -> "COMMENT " <> (quoteString defaultParams) t
	Collate i -> "COLLATE " <> i
	-- MySQLisms
	Unsigned -> "UNSIGNED"
	AutoIncrement -> "AUTO_INCREMENT"
	CharSet i -> "CHARSET " <> i
	OnUpdate expr -> "ON UPDATE " <> expr

--------------------------------------------------------------------- | Comments

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
		, (quoteString defaultParams) c
		, "; */"
		]

{----------------------------------------------------------------------------------------------------{
																	  | Inserts
}----------------------------------------------------------------------------------------------------}



{----------------------------------------------------------------------------------------------------{
																	  | Indexes, Constraints, etc.
}----------------------------------------------------------------------------------------------------}

{----------------------------------------------------------------------------------------------------{
																	  | Helper Functions
}----------------------------------------------------------------------------------------------------}

removeEmpties :: [BS.ByteString] -> [BS.ByteString]
removeEmpties = filter (/= "")

quoteIdentifier' :: Char -> BS.ByteString -> BS.ByteString
quoteIdentifier' quoteChar x = BS.cons quoteChar $ BS.snoc x quoteChar

quoteString' :: Char -> Char -> BS.ByteString -> BS.ByteString
quoteString' quoteChar escapeChar =
	quoteIdentifier' quoteChar . BS.toStrict . BS.replace (BS.singleton quoteChar) (BS.pack [quoteChar, escapeChar])

toCSV :: [BS.ByteString] -> BS.ByteString
toCSV = BS.intercalate ", "

constructIdentifierToText :: ConstructIdentifier -> BS.ByteString
constructIdentifierToText (ConstructIdentifier xs) = BS.intercalate "." $ map (quoteIdentifier defaultParams) xs

extractFromList :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractFromList f xs =
	let (x, rest) = partition f xs
	in (listToMaybe x, rest)

{----------------------------------------------------------------------------------------------------{
																	  | Partitioning functions
}----------------------------------------------------------------------------------------------------}

partitionTables :: (TableAttribute -> Bool) -> [Construct] -> ([Construct], [Construct])
partitionTables f = unzip . mapMaybe isMatch
	where
		isMatch (Table n cx) =
			let (match, rest) = partition f cx
			in Just (Table n match, Table n rest)
		isMatch _ = Nothing

-- Foreign Keys need to be placed at the end to ensure that the unique
-- constraints (Primary Key/Unique) exist
partitionConstraints :: [Construct] -> ([Construct], [Construct])
partitionConstraints xs =
	unzip $ mapMaybe justConstraints xs
	where
		justConstraints (Table n cx) =
			let (fk, rest) = partition isForeignKey $ filter isConstraint cx
			in Just (Table n fk, Table n rest)
		justConstraints _ = Nothing
