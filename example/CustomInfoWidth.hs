{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-- | Custom info width example.
module Main (main) where

import Env
import System.Environment (getArgs)
import Text.Read (readMaybe)


main :: IO ()
main = do
  (readMaybe -> Just widthMax) : _ <- getArgs
  _ <- hello widthMax
  error "impossible"

hello :: Int -> IO ()
hello n = Env.parse (header "envparse example" . Env.widthMax n) $ do
  _ <- var (str @String) "NAME" (help loremIpsum)
  _ <- var (str @String) "A_NAME_THAT_DOESN'T_FIT_IN_THE_COMPACT_VIEW" (help loremIpsum)
  pure ()

loremIpsum :: String
loremIpsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
  \sed do eiusmod tempor incididunt ut labore et dolore magna \
  \aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco \
  \laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure \
  \dolor in reprehenderit in voluptate velit esse cillum dolore eu \
  \fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \
  \proident, sunt in culpa qui officia deserunt mollit anim id est \
  \laborum."
