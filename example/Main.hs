{-# LANGUAGE NamedFieldPuns #-}
-- | Greetings for $NAME
--
-- @
-- % NAME=a5579150 runhaskell -isrc example/Main.hs
-- Hello, a5579150!
-- @
module Main (main) where

import Env


newtype Hello = Hello { name :: String }


main :: IO ()
main = do
  Hello { name } <- hello
  putStrLn ("Hello, " ++ name ++ "!")

hello :: IO Hello
hello = Env.parse (header "envparse example") $
  Hello <$> var (str <=< nonempty) "NAME" (help "Target for the greeting")
