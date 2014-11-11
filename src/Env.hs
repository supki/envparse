{-# LANGUAGE Safe #-}
-- | Here's a simple example
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
-- hello = 'Env.parse' ('header' \"envparse example\") $
--   Hello \<$\> 'var' ('str' <=< 'nonempty') \"NAME\"  ('help' \"Target for the greeting\")
--         \<*\> 'switch'                 \"QUIET\" ('help' \"Whether to actually print the greeting\")
--
-- main :: IO ()
-- main = do
--   Hello { name, quiet } <- hello
--   unless quiet $
--     putStrLn (\"Hello, \" ++ name ++ \"!\")
-- @
module Env
  ( parse
  , Parser
  , Mod
  , Info
  , header
  , desc
  , footer
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
  , (<$>), Applicative(..), optional
  , (<=<), (>=>)
  , (<>), mempty, mconcat
  , asum
  -- * Testing
  -- $testing
  , fromEnv
  ) where

import           Control.Applicative
import           Control.Monad ((>=>), (<=<))
import           Data.Foldable (asum)
import           Data.Monoid (Monoid(..), (<>))
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure)
import qualified System.IO as IO

import           Env.Help (helpDoc)
import           Env.Parse

-- $re-exports
-- External functions that may be useful to the consumer of the library

-- $testing
-- Utilities to test—without dabbling in IO—that your parsers do
-- what you want them to do


-- | Parse the environment
--
-- Prints the help text and exits with @EXIT_FAILURE@ if it encounters a parse error
--
-- @
-- >>> parse ('header' \"env-parse 0.1.0\") ('var' 'str' \"USER\" ('def' \"nobody\"))
-- @
parse :: Mod Info a -> Parser a -> IO a
parse i p = either (die . helpDoc i p) return . fromEnv p =<< getEnvironment

die :: String -> IO a
die m = do IO.hPutStrLn IO.stderr m; exitFailure
