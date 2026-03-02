{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
-- | Greetings for $NAME
--
-- @
-- % NAME=a5579150 runhaskell -isrc example/Modern.hs
-- Hello, a5579150!
-- % NAME=a5579150 QUIET=1 runhaskell -isrc example/Modern.hs
-- %
-- @
module Main (main) where

import Control.Monad (unless)
import Env


data Hello = Hello { name :: String, quiet :: Bool }

main :: IO ()
main = do
  hello <- parseHello
  unless hello.quiet $
    putStrLn ("Hello, " ++ hello.name ++ "!")

parseHello :: IO Hello
parseHello =
  Env.parse (header "envparse example") $ do
    name <-
      var (str <=< nonempty) "NAME"  (help "Target for the greeting")
    quiet <-
      switch                 "QUIET" (help "Whether to actually print the greeting")
    pure Hello {..}
