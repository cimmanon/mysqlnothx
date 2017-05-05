{-# LANGUAGE OverloadedStrings #-}

module Parser.Flow where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Monoid ((<>))
import Control.Applicative

import Prelude hiding (concat, takeWhile)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS

import Data.Construct
import Parser.Common
import Parser.Construct
import Parser.Data

{----------------------------------------------------------------------------------------------------{
                                                                      | Type
}----------------------------------------------------------------------------------------------------}

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)

{----------------------------------------------------------------------------------------------------{
                                                                      | Main Parser
}----------------------------------------------------------------------------------------------------}

dump :: StatefulParser Command
dump = createStatements *> fmap Insert justData

createStatements :: StatefulParser [Construct]
createStatements = do
	statements <- lift collectStatements
	currentState <- get
	_ <- put (currentState { constructs = statements ++ (constructs currentState)})
	return statements
	where
		statement = catchStatements <* skipMany skipStatements
		collectStatements = regroupTables <$> do
			skipMany skipStatements
			many1 statement
			<?> "collect create statements"

catchStatements :: Parser Construct
catchStatements = createTable <|> createIndex <|> alterTable

skipStatements :: Parser BS.ByteString
skipStatements = emptyStatement <|> comment <|> dropTable <|> lockTable <|> unlockLockTable <|> transactions <|> set
	where
		transactions = (string "BEGIN" <|> string "COMMIT" <|> string "ROLLBACK") <* endOfStatement
		emptyStatement = endOfStatement *> return ""
		-- MySQL
		dropTable = string "DROP TABLE" *> takeWhile (isNot ';') <* endOfStatement
		lockTable = string "LOCK TABLES" *> takeWhile (isNot ';') <* endOfStatement
		unlockLockTable = string "UNLOCK TABLES;" *> skipSpace *> return ""
		set = string "SET" *> takeWhile1 (isNot ';') <* endOfStatement

-- I really wish I wrote a comment explaining what's going on here when I wrote it.
-- Sorting by table name, then grouping together, then merging attributes
-- from grouped tables, and finally concatting them back into a single list.
-- but why???  I think it might have to do with grouping indexes with their
-- associated table
regroupTables :: [Construct] -> [Construct]
regroupTables = reduce . regroup
	where
		regroup = groupBy ((==) `on` name) . sortBy (compare `on` name)
		reduce = map (foldl1 (\ (Table n x) (Table _ y) -> Table n $ x ++ y))

-- handles the insert statements by converting them to the COPY command
justData :: StatefulParser BS.ByteString
justData = lift (skipMany skipStatements) *> insertToCopy <* lift (skipMany skipStatements)
