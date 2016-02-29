{-# LANGUAGE NamedFieldPuns #-}
-- | Greetings for $NAMES
--
-- @
-- % NAMES=foo runhaskell -isrc example/Main.hs
-- ...
-- NAMES should have between 3 and 4 names, but there's 2 of them
-- % NAMES=foo,bar,baz runhaskell -isrc example/Main.hs
-- Hello, foo!
-- Hello, bar!
-- Hello, baz!
module Main (main) where

import           Control.Monad (forM_)
import           Env
import qualified Env.Error as Error
import           Text.Printf (printf)


newtype Hello = Hello { names :: [String] }


main :: IO ()
main = do
  Hello {names} <- hello
  forM_ names $ \name ->
    putStrLn ("Hello, " ++ name ++ "!")

hello :: IO Hello
hello = Env.parseWith handleCustomError (header "envparse example") $ Hello
  <$> var (goodEnough <=< sepBy ',' <=< nonempty) "NAMES" (help "Targets for the greeting")

handleCustomError :: ErrorHandler CustomError
handleCustomError name err =
  case err of
    LengthError l (lmin, lmax) ->
      Just (printf "  %s should have between %d and %d names, but there's %d of them" name lmin lmax l)
    _ ->
      handleError name err

goodEnough :: [String] -> Either CustomError [String]
goodEnough xs
  | l < lmin || l > lmax =
    Left (LengthError l range)
  | otherwise =
    pure xs
 where
  l = length xs
  range@(lmin, lmax) = (3, 4)

sepBy :: Char -> Reader e [String]
sepBy sep =
  pure . splitOn sep

data CustomError
  = LengthError Int (Int, Int)
  | EnvError Error

-- * Boilerplate

instance AsUnset CustomError where
  unset =
    EnvError Error.unset
  tryUnset err =
    case err of
      EnvError err' -> Error.tryUnset err'
      _ -> Nothing

instance Error.AsEmpty CustomError where
  empty =
    EnvError Error.empty
  tryEmpty err =
    case err of
      EnvError err' -> Error.tryEmpty err'
      _ -> Nothing

instance Error.AsInvalid CustomError where
  invalid =
    EnvError . Error.invalid
  tryInvalid err =
    case err of
      EnvError err' -> Error.tryInvalid err'
      _ -> Nothing

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = go
 where
  go xs =
    case break (== sep) xs of
      ([], _) -> []
      (ys, []) -> ys : []
      (ys, _ : zs) -> ys : go zs
