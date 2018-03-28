{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-- | Here's a simple example of a program that uses @envparse@'s parser:
--
-- @
-- module Main (main) where
--
-- import Control.Monad (unless)
-- import Env
--
-- data Hello = Hello { name :: String, quiet :: Bool }
--
-- hello :: IO Hello
-- hello = Env.'parse' ('Help.header' \"envparse example\") $
--   Hello \<$\> 'var' ('str' <=< 'nonempty') \"NAME\"  ('help' \"Target for the greeting\")
--         \<*\> 'switch'                 \"QUIET\" ('help' \"Whether to actually print the greeting\")
--
-- main :: IO ()
-- main = do
--   Hello {name, quiet} <- hello
--   unless quiet $
--     putStrLn (\"Hello, \" ++ name ++ \"!\")
-- @
--
-- The @NAME@ environment variable is mandatory and contains the name of the person to
-- greet. @QUIET@, on the other hand, is an optional boolean flag, false by default, that
-- decides whether the greeting should be silent.
--
-- If the @NAME@ variable is undefined in the environment then running the program will
-- result in the following help text:
--
-- @
-- envparse example
--
-- Available environment variables:
--
--   NAME                   Target for the greeting
--   QUIET                  Whether to actually print the
--                          greeting
--
-- Parsing errors:
--
--   NAME is unset
-- @
module Env
  ( parse
  , parseOr
  , Parser
  , Mod
  , Help.Info
  , Help.header
  , Help.desc
  , Help.footer
  , Help.handleError
  , Help.ErrorHandler
  , Help.defaultErrorHandler
  , prefixed
  , var
  , Var
  , Reader
  , str
  , nonempty
  , splitOn
  , auto
  , def
  , helpDef
  , flag
  , switch
  , Flag
  , HasHelp
  , help
  , HasClear
  , clear
  , Help.helpDoc
  , Error(..)
  , Error.AsUnset(..)
  , Error.AsEmpty(..)
  , Error.AsUnread(..)
  -- * Re-exports
  -- $re-exports
  , optional, (<=<), (>=>), (<>), asum
  -- * Testing
  -- $testing
  , parsePure
  ) where

import           Control.Applicative
import           Control.Monad ((>=>), (<=<))
import           Data.Foldable (asum, for_)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..), (<>))
#else
import           Data.Monoid ((<>))
#endif
import           System.Environment (getEnvironment)
#if __GLASGOW_HASKELL__ >= 708
import           System.Environment (unsetEnv)
#endif
import           System.Exit (exitFailure)
import qualified System.IO as IO

import qualified Env.Internal.Help as Help
import           Env.Internal.Parser
import           Env.Internal.Error (Error)
import qualified Env.Internal.Error as Error

-- $re-exports
-- External functions that may be useful to the consumer of the library

-- $testing
-- Utilities to test—without dabbling in IO—that your parsers do
-- what you want them to do

-- | Parse the environment or die
--
-- Prints the help text and exits with @EXIT_FAILURE@ on encountering a parse error.
--
-- @
-- >>> parse ('Help.header' \"env-parse 0.2.0\") ('var' 'str' \"USER\" ('def' \"nobody\"))
-- @
parse :: (Help.Info Error -> Help.Info e) -> Parser e a -> IO a
parse m =
  fmap (either (\_ -> error "absurd") id) . parseOr die m

-- | Try to parse the environment
--
-- Use this if simply dying on failure (the behavior of 'parse') is inadequate for your needs.
parseOr :: (String -> IO a) -> (Help.Info Error -> Help.Info e) -> Parser e b -> IO (Either a b)
parseOr onFailure helpMod parser = do
  b <- fmap (parsePure parser) getEnvironment
#if __GLASGOW_HASKELL__ >= 708
  for_ b $ \_ ->
    eachUnsetVar parser unsetEnv
#endif
  traverseLeft (onFailure . Help.helpInfo (helpMod Help.defaultInfo) parser) b

die :: String -> IO a
die m =
  do IO.hPutStrLn IO.stderr m; exitFailure

traverseLeft :: Applicative f => (a -> f b) -> Either a t -> f (Either b t)
traverseLeft f =
  either (fmap Left . f) (pure . Right)
