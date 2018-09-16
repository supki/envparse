{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (unless)
import Env


data Hello = Hello { name :: String, quiet :: Bool }

main :: IO ()
main = do
  Hello {name, quiet} <- hello
  unless quiet $
    putStrLn ("Hello, " ++ name ++ "!")

hello :: IO Hello
hello = Env.parse (header "envparse example" . varHelpMaxColumns 99 . varNameColumn 2 . varHelpColumn 8) $
  Hello <$> var (str <=< nonempty) "NAME"  (help "Target for the greeting. The greeting will say hello, comma, <NAME>, and a final exclamation")
        <*> switch                 "QUIET" (help "Whether to actually print the greeting. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
