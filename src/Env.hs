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
-- hello = Env.'parse' ('header' \"envparse example\") $
--   Hello \<$\> 'var' ('str' <=< 'nonempty') \"NAME\"  ('help' \"Target for the greeting\")
--         \<*\> 'switch'                 \"QUIET\" ('help' \"Whether to actually print the greeting\")
--
-- main :: IO ()
-- main = do
--   Hello { name, quiet } <- hello
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
  , Info
  , header
  , desc
  , footer
  , prefixed
  , var
  , Var
  , Reader
  , str
  , nonempty
  , auto
  , def
  , helpDef
  , flag
  , switch
  , Flag
  , HasHelp
  , help
  , helpDoc
  -- * Re-exports
  -- $re-exports
  , pure, (<$>), (<*>), (*>), (<*), optional
  , empty, (<|>)
  , (<=<), (>=>)
  , (<>), mempty, mconcat
  , asum
  -- * Testing
  -- $testing
  , parsePure
  ) where

import           Control.Applicative
import           Control.Monad ((>=>), (<=<))
import           Data.Foldable (asum)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.Monoid ((<>))
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Env.Help (helpInfo, helpDoc)
import           Env.Parse

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
-- >>> parse ('header' \"env-parse 0.2.0\") ('var' 'str' \"USER\" ('def' \"nobody\"))
-- @
parse :: Mod Info a -> Parser a -> IO a
parse m = fmap (either (\_ -> error "absurd") id) . parseOr die m

-- | Try to parse the environment
--
-- Use this if simply dying on failure (the behavior of 'parse') is inadequate for your needs.
parseOr :: (String -> IO a) -> Mod Info b -> Parser b -> IO (Either a b)
parseOr f m p = traverseLeft f . parsePure m p =<< getEnvironment

die :: String -> IO a
die m = do IO.hPutStrLn IO.stderr m; exitFailure

-- | Try to parse a pure environment
parsePure :: Mod Info a -> Parser a -> [(String, String)] -> Either String a
parsePure (Mod f) p = mapLeft (helpInfo (f defaultInfo) p) . static p

mapLeft :: (a -> b) -> Either a t -> Either b t
mapLeft f = either (Left . f) Right

traverseLeft :: Applicative f => (a -> f b) -> Either a t -> f (Either b t)
traverseLeft f = either (fmap Left . f) (pure . Right)
