{-# LANGUAGE OverloadedStrings #-}

module Data.Construct where

import Control.Applicative ((<$>))
import Data.List (find)
import Data.Maybe (isJust)
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

newtype ParserState = ParserState { constructs :: [Construct] }

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
--  | Type
--  | Function
    deriving (Show, Eq, Ord)

data TableAttribute =
      Column Identifier Type [ColumnAttribute]
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

{----------------------------------------------------------------------------------------------------{
                                                                      | Columns
}----------------------------------------------------------------------------------------------------}

data ColumnAttribute =
      CharSet Type -- MySQLism
    | Collate Type
    | Unsigned -- MySQLism
    | Nullable Bool
    | Default ByteString
    | PrimaryKey' -- This shouldn't be in a dump file
    | Unique' -- This shouldn't be in a dump file
    | AutoIncrement -- MySQLism
    | Comment' ByteString -- MySQLism
    | OnUpdate ByteString -- MySQLism
    deriving (Show, Eq, Ord)

isCharSet :: ColumnAttribute -> Bool
isCharSet (CharSet _) = True
isCharSet _ = False

isCollate :: ColumnAttribute -> Bool
isCollate (Collate _) = True
isCollate _ = False

isUnsigned :: ColumnAttribute -> Bool
isUnsigned Unsigned = True
isUnsigned _ = False

isDefault :: ColumnAttribute -> Bool
isDefault (Default _) = True
isDefault _ = False

isPrimaryKey' :: ColumnAttribute -> Bool
isPrimaryKey' PrimaryKey' = True
isPrimaryKey' _ = False

isUnique' :: ColumnAttribute -> Bool
isUnique' Unique' = True
isUnique' _ = False

isAutoIncrement :: ColumnAttribute -> Bool
isAutoIncrement AutoIncrement = True
isAutoIncrement _ = False

isComment' :: ColumnAttribute -> Bool
isComment' (Comment' _) = True
isComment' _ = False

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
            PrimaryKey' -> True
            _ -> False
isPrimaryKey _ = False

--

isUnique :: TableAttribute -> Bool
isUnique (Constraint _ (Unique _)) = True
isUnique (Column _ _ xs) = has isUK xs
    where
        isUK x = case x of
            Unique' -> True
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
