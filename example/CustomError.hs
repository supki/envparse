{-# LANGUAGE NamedFieldPuns #-}
-- | Greetings for $NAMES
--
-- @
-- % NAME=a5579150 COUNT=0 runhaskell -isrc example/Main.hs
-- ...
-- COUNT must be > 0, but is 0
-- % NAME=a5579150 COUNT=3 runhaskell -isrc example/Main.hs
-- Hello, foo!
-- Hello, foo!
-- Hello, foo!
module Main (main) where

import           Control.Category (Category(..))
import           Control.Monad (replicateM_)
import           Env
import qualified Env.Error as Error
import           Prelude hiding ((.), id)
import           Text.Printf (printf)


data Hello = Hello { name :: String, count :: Int }

main :: IO ()
main = do
  Hello {name, count} <- hello
  replicateM_ count $
    putStrLn ("Hello, " ++ name ++ "!")

hello :: IO Hello
hello = Env.parse (header "envparse example" . handleError customErrorHandler) $ Hello
  <$> var nonempty            "NAME"  (help "Target for the greeting")
  <*> var (positive <=< auto) "COUNT" (help "How many times to greet?")

customErrorHandler :: ErrorHandler CustomError
customErrorHandler name err =
  case err of
    NonPositive n ->
      Just (printf "  %s must be > 0, but is %d" name n)
    _ ->
      defaultErrorHandler name err

positive :: Int -> Either CustomError Int
positive n
  | n <= 0 =
    Left (NonPositive n)
  | otherwise =
    pure n

data CustomError
  = NonPositive Int
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
