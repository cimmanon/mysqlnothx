{-# LANGUAGE OverloadedStrings #-}

module Data.Construct where

import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
import Data.ByteString (ByteString)

{----------------------------------------------------------------------------------------------------{
                                                                      | Types
}----------------------------------------------------------------------------------------------------}

-- Identifiers are for names (tables, columns, foreign keys, etc.)
-- the quotes should be stripped out so that the correct quotes can be
-- applied when the import file is generated
type Identifier = ByteString

-- Column/Argument type
-- this type should never be quoted, but may contain parentheses or square brackets
-- eg. ByteString, INT[], FOO(INT, BAR(ByteString, ByteString[]))
type Type = ByteString

--type QuotedByteString = ByteString

-- TODO: turn this into a Monoid
data ConstructIdentifier = ConstructIdentifier [Identifier] deriving (Show, Eq, Ord)

appendIdentifier :: ConstructIdentifier -> Identifier -> ConstructIdentifier
appendIdentifier (ConstructIdentifier xs) x = ConstructIdentifier $ xs ++ [x]

newtype ParserState = ParserState { constructs :: [Construct] } deriving (Show)

defaultParserState :: ParserState
defaultParserState = ParserState []

{----------------------------------------------------------------------------------------------------{
                                                                      | Command
}----------------------------------------------------------------------------------------------------}

data Command = Create [Construct] | Insert ByteString deriving (Show, Eq, Ord)

{----------------------------------------------------------------------------------------------------{
                                                                      | Constructs
}----------------------------------------------------------------------------------------------------}

data Construct =
	  Table  { name :: ConstructIdentifier, body :: [TableAttribute] }
	| Comment Type ConstructIdentifier ByteString
	| Sequence { n :: Identifier, start :: Int, increment :: Int, ownedBy :: Maybe ConstructIdentifier }
--	| Type
--	| Function
	deriving (Show, Eq, Ord)

data TableAttribute =
	  Column Identifier Scalar [ColumnAttribute]
	| Index (Maybe Identifier) [(Identifier, Maybe ByteString)] (Maybe Type) -- name, condition, type
	| Constraint (Maybe Identifier) ConstraintType
	deriving (Show, Eq, Ord)

isTable :: Construct -> Bool
isTable (Table _ _) = True
isTable _ = False

isSequence :: Construct -> Bool
isSequence (Sequence {}) = True
isSequence _ = False

{----------------------------------------------------------------------------------------------------{
                                                                      | Tables
}----------------------------------------------------------------------------------------------------}

isColumn :: TableAttribute -> Bool
isColumn (Column {}) = True
isColumn _ = False

isConstraint :: TableAttribute -> Bool
isConstraint (Constraint {}) = True
isConstraint _ = False

isIndex :: TableAttribute -> Bool
isIndex (Index {}) = True
isIndex _ = False

columnTypes :: [TableAttribute] -> [Scalar]
columnTypes = mapMaybe getType
	where
		getType (Column _ s _) = Just s
		getType _ = Nothing

{----------------------------------------------------------------------------------------------------{
                                                                      | Columns
}----------------------------------------------------------------------------------------------------}

type Bytes = Int
type Length = Int
type Signed = Bool

data Scalar =
	  Text Bytes
	| Char Length
	| Varchar Length
	| Bit Int
	| Blob Bytes -- Binary data
	| Numeric (Int, Int) Signed
	| Float Signed -- 4 bytes
	| Double Signed -- 8 bytes
	| Integer Bytes Signed
	| Timestamp
	| Time
	| Date
	| Unknown ByteString
	deriving (Show, Eq, Ord)
	-- TODO: Set, Enum
	-- http://www.rdeeson.com/weblog/88/enums-user-preferences-and-the-mysql-set-datatype.html

data ColumnAttribute =
	  Nullable Bool
	| Default ByteString
	| Collate Type
	| CharSet Type -- MySQLism
	| AutoIncrement -- MySQLism
	| OnUpdate ByteString -- MySQLism
	| InlineComment ByteString -- MySQLism
	| InlinePrimaryKey -- This shouldn't be in a dump file
	| InlineUnique -- This shouldn't be in a dump file
	deriving (Show, Eq, Ord)

isCharSet :: ColumnAttribute -> Bool
isCharSet (CharSet _) = True
isCharSet _ = False

isCollate :: ColumnAttribute -> Bool
isCollate (Collate _) = True
isCollate _ = False

isDefault :: ColumnAttribute -> Bool
isDefault (Default _) = True
isDefault _ = False

isInlinePrimaryKey :: ColumnAttribute -> Bool
isInlinePrimaryKey InlinePrimaryKey = True
isInlinePrimaryKey _ = False

isInlineUnique :: ColumnAttribute -> Bool
isInlineUnique InlineUnique = True
isInlineUnique _ = False

isAutoIncrement :: ColumnAttribute -> Bool
isAutoIncrement AutoIncrement = True
isAutoIncrement _ = False

isInlineComment :: ColumnAttribute -> Bool
isInlineComment (InlineComment _) = True
isInlineComment _ = False

isOnUpdate :: ColumnAttribute -> Bool
isOnUpdate (OnUpdate _) = True
isOnUpdate _ = False

{----------------------------------------------------------------------------------------------------{
                                                                      | Constraints
}----------------------------------------------------------------------------------------------------}

data ConstraintType =
	  PrimaryKey [Identifier]
	| Unique [Identifier]
	| ForeignKey [Identifier] (ConstructIdentifier, [Identifier]) (Maybe ByteString) (Maybe ByteString)
	| Check ByteString
	deriving (Show, Eq, Ord)

isConstraint' :: TableAttribute -> Bool
isConstraint' x =  isConstraint x || isPrimaryKey x || isUnique x

--

isPrimaryKey :: TableAttribute -> Bool
isPrimaryKey (Constraint _ (PrimaryKey _)) = True
isPrimaryKey (Column _ _ xs) = has isPK xs
	where
		isPK x = case x of
			InlinePrimaryKey -> True
			_ -> False
isPrimaryKey _ = False

--

isUnique :: TableAttribute -> Bool
isUnique (Constraint _ (Unique _)) = True
isUnique (Column _ _ xs) = has isUK xs
	where
		isUK x = case x of
			InlineUnique -> True
			_ -> False
isUnique _ = False

--

isForeignKey :: TableAttribute -> Bool
isForeignKey (Constraint _ (ForeignKey {})) = True
isForeignKey _ = False

--

isCheck :: TableAttribute -> Bool
isCheck (Constraint _ (Check _)) = True
isCheck _ = False

{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

has :: (a -> Bool) -> [a] -> Bool
has f x = isJust $ find f x

hasAutoIncrement :: TableAttribute -> Bool
hasAutoIncrement (Column _ _ xs) = has isAutoIncrement xs
hasAutoIncrement _ = False
