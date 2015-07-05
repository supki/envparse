{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
module Main (main) where

main :: IO ()
main =
  return ()
#else
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
-- | Greetings for $NAME
--
-- @
-- % GENERIC_NAME=a5579150 runhaskell -isrc example/Generic.hs
-- Hello, a5579150!
-- % GENERIC_NAME=a5579150 GENERIC_QUIET=1 runhaskell -isrc example/Generic.hs
-- %
-- @
module Main (main) where

import Control.Monad (unless)
import Env
import Env.Generic


data Hello = Hello
  { name  :: String ? "Target for the greeting"
  , quiet :: Bool   ? "Whether to actually print the greeting"
  } deriving (Show, Eq, Generic)

instance Record Error Hello

main :: IO ()
main = do
  Hello {name=Help n, quiet=Help q} <- hello
  unless q $
    putStrLn ("Hello, " ++ n ++ "!")

hello :: IO Hello
hello =
  Env.parse (header "envparse example") (prefixed "GENERIC_" record)
#endif
