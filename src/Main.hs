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
import Pipes as P (runEffect, for, liftIO, Producer, Effect)
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

runParser :: MonadIO m => Producer Command (StateT ParserState m) (Either (ParsingError, Producer ByteString (StateT ParserState m) ()) ())
runParser = do
	xs <- constructs <$> lift get
	parsed (dump'' xs) PBS.stdin

processCommand :: Monad m => Command -> StateT ParserState m ByteString
processCommand (Create xs) = do
	currentState <- get
	_ <- put (currentState { constructs = xs ++ (constructs currentState)})
	return $ P.firstPass $ P.transformConstructs xs
processCommand (Insert x) = return x
