{-# LANGUAGE OverloadedStrings #-}

module Output where

--import Control.Monad.Reader
import Data.List (find, partition)
import Data.Maybe (listToMaybe, mapMaybe, isJust)
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

{----------------------------------------------------------------------------------------------------{
                                                                      | Helper Functions
}----------------------------------------------------------------------------------------------------}

removeEmpties :: [BS.ByteString] -> [BS.ByteString]
removeEmpties = filter (/= "")

quoteIdentifier :: Char -> BS.ByteString -> BS.ByteString
quoteIdentifier quoteChar x = BS.cons quoteChar $ BS.snoc x quoteChar

quoteString :: Char -> Char -> BS.ByteString -> BS.ByteString
quoteString quoteChar escapeChar =
	quoteIdentifier quoteChar . BS.toStrict . BS.replace (BS.singleton quoteChar) (BS.pack [quoteChar, escapeChar])

toCSV :: [BS.ByteString] -> BS.ByteString
toCSV = BS.intercalate ", "

-- not used
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

{-
There are 2 instances where a constraint would be considered imaginary:

* The table doesn't exist
* The table exists, but the columns don't have a unique constraint on them

Both are valid in MySQL, but invalid elsewhere
-}

isRealConstraint :: [Construct] -> TableAttribute -> Bool
isRealConstraint xs (Constraint _ (ForeignKey _ (t, cols) _ _)) =
	let
		isUniqueConstraint (Constraint _ (Unique cs)) = cs == cols
		isUniqueConstraint (Constraint _ (PrimaryKey cs)) = cs == cols
		isUniqueConstraint _ = False
	in
		isJust $ find (\ x -> t == name x && not (null $ filter isUniqueConstraint $ body x)) xs
isRealConstraint _ _ = False
