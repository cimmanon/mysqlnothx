{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Common
import Control.Applicative ((<$>))
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)

-- IO
import qualified Data.ByteString.Char8 as BS (putStrLn)
import System.Exit (ExitCode (..), exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

-- Pipes
import Pipes (runEffect, for, liftIO, Producer, Effect)
import Pipes.Attoparsec (parsed, ParsingError)
import Pipes.Lift (runStateP)
import Pipes.Safe (runSafeT)
import qualified Pipes.ByteString as PBS (stdin)

-- State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- local imports
import Data.Construct
import Parser.Flow

import qualified Output.PostgreSQL as P

import qualified Data.ByteString as BS (concat)

main :: IO ExitCode
main = do
	-- begin our transaction
	putStrLn "\\set ON_ERROR_STOP true\n\n"
	putStrLn "BEGIN;\n"

	-- run the parsers that generate our create/insert statements
	(r, (ParserState xs)) <- runSafeT $ runEffect doStuff

	-- create our indexes/constraints at the very end
	BS.putStrLn $ P.secondPass xs
--	putStrLn "ROLLBACK;\n"

	-- cleanup by sending a proper exit code
	case r of
		Right _ -> exitSuccess
		Left (parseErr, _) -> do
			hPutStrLn stderr $ "Parse error somewhere\n" ++ show parseErr
			when (not $ null xs) $ hPutStrLn stderr $ "Last successful construct parsed: " ++ (show $ head xs)
			exitFailure

----------------------------------------------------------------------
-- These functions are split out so that it's easier to typecheck them

doStuff :: MonadIO m => Effect m (Either (ParsingError, Producer ByteString (StateT ParserState m) ()) (), ParserState)
doStuff = runStateP defaultParserState theStuff

theStuff :: MonadIO m => Effect (StateT ParserState m) (Either (ParsingError, Producer ByteString (StateT ParserState m) ()) ())
theStuff = for runParser (liftIO . BS.putStrLn <=< lift . processCommand)

{-
runParser :: MonadIO m => Producer Command (StateT ParserState m) (Either (ParsingError, Producer ByteString (StateT ParserState m) ()) ())
runParser = do
	s <- lift get
	-- TODO: figure out how to get the return state out to pass it back into the parent monad's state
	parsed (evalStateT dump s) PBS.stdin
-}
runParser :: MonadIO m => Producer (Command, ParserState) (StateT ParserState m) (Either (ParsingError, Producer ByteString (StateT ParserState m) ()) ())
runParser = do
	s <- lift get
	-- TODO: figure out how to get the return state out to pass it back into the parent monad's state
	parsed (runStateT dump s) PBS.stdin

{-
processCommand :: Monad m => Command -> StateT ParserState m ByteString
processCommand (Create xs) = do
	currentState <- get
	_ <- put (currentState { constructs = xs ++ (constructs currentState)})
	return $ P.firstPass $ P.transformConstructs xs
processCommand (Insert x) = return x
-}
processCommand :: Monad m => (Command, ParserState) -> StateT ParserState m ByteString
processCommand (Create _, ParserState xs') = do
	currentState <- get
	_ <- put (currentState { constructs = xs' ++ (constructs currentState)})
	return $ P.firstPass $ P.transformConstructs xs'
processCommand (Insert x, ParserState xs) = do
	currentState <- get
	_ <- put (currentState { constructs = xs ++ (constructs currentState)})
	return $ BS.concat [ P.firstPass $ P.transformConstructs xs, "\n\n", x ]
