{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif
import           Control.Monad (replicateM_)
import           Env
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
    return n

data CustomError
  = NonPositive Int
  | EnvError Error

-- * Boilerplate

instance AsUnset CustomError where
  unset =
    EnvError unset
  tryUnset err =
    case err of
      EnvError err' -> tryUnset err'
      _ -> Nothing

instance AsEmpty CustomError where
  empty =
    EnvError empty
  tryEmpty err =
    case err of
      EnvError err' -> tryEmpty err'
      _ -> Nothing

instance AsUnread CustomError where
  unread =
    EnvError . unread
  tryUnread err =
    case err of
      EnvError err' -> tryUnread err'
      _ -> Nothing
