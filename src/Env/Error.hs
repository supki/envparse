module Env.Error
  ( Error(..)
  , AsUnset(..)
  , AsEmpty(..)
  , AsInvalid(..)
  ) where


data Error
  = UnsetError String
  | EmptyError String
  | InvalidError String String
    deriving (Show, Eq)

instance AsUnset Error where
  unset = UnsetError

instance AsEmpty Error where
  empty = EmptyError

instance AsInvalid Error where
  invalid = InvalidError

class AsUnset e where
  unset :: String -> e

class AsEmpty e where
  empty :: String -> e

class AsInvalid e where
  invalid :: String -> String -> e
