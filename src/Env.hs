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
--   NAME is missing
-- @
module Env
  ( parse
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
  -- * Re-exports
  -- $re-exports
  , pure, (<$>), (<*>), (*>), (<*), optional
  , empty, (<|>)
  , (<=<), (>=>)
  , (<>), mempty, mconcat
  , asum
  -- * Testing
  -- $testing
  , parseTest
  ) where

import           Control.Applicative
import           Control.Monad ((>=>), (<=<))
import           Data.Foldable (asum)
import           Data.Monoid (Monoid(..), (<>))
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Env.Free (hoistAlt)
import           Env.Help (helpDoc)
import           Env.Parse

-- $re-exports
-- External functions that may be useful to the consumer of the library

-- $testing
-- Utilities to test—without dabbling in IO—that your parsers do
-- what you want them to do


-- | Succesfully parse the environment or die
--
-- Prints the help text and exits with @EXIT_FAILURE@ on encountering a parse error
--
-- @
-- >>> parse ('header' \"env-parse 0.1.0\") ('var' 'str' \"USER\" ('def' \"nobody\"))
-- @
parse :: Mod Info a -> Parser a -> IO a
parse (Mod f) (Parser p) = either (die . helpDoc i p') return . static p' =<< getEnvironment
 where
  i = f defaultInfo
  p' = Parser (maybe p (\pre -> hoistAlt (\v -> v { varfName = pre ++ varfName v }) p) (infoPrefixed i))

die :: String -> IO a
die m = do IO.hPutStrLn IO.stderr m; exitFailure

-- | Parse a static environment
parseTest :: Mod Info a -> Parser a -> [(String, String)] -> Maybe a
parseTest (Mod f) (Parser p) = hush . static p'
 where
  i = f defaultInfo
  p' = Parser (maybe p (\pre -> hoistAlt (\v -> v { varfName = pre ++ varfName v }) p) (infoPrefixed i))

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
