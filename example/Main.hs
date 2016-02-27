{-# LANGUAGE NamedFieldPuns #-}
-- | Greetings for $NAME
--
-- @
-- % NAME=a5579150 runhaskell -isrc example/Main.hs
-- Hello, a5579150!
-- % NAME=a5579150 QUIET=1 runhaskell -isrc example/Main.hs
-- %
-- @
module Main (main) where

import Control.Monad (unless)
import Env


data Hello = Hello { name :: String, quiet :: Bool }

main :: IO ()
main = do
  Hello {name, quiet} <- hello
  unless quiet $
    putStrLn ("Hello, " ++ name ++ "!")

hello :: IO Hello
hello = Env.parse (header "envparse example") $
  Hello <$> var (str <=< nonempty) "NAME"  (help "Target for the greeting")
        <*> switch                 "QUIET" (help "Whether to actually print the greeting")
