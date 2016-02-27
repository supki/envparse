module Env.Error
  ( Error(..)
  , AsUnset(..)
  , AsEmpty(..)
  , AsInvalid(..)
  ) where


data Error
  = UnsetError
  | EmptyError
  | InvalidError String
    deriving (Show, Eq)

instance AsUnset Error where
  unset = UnsetError

instance AsEmpty Error where
  empty = EmptyError

instance AsInvalid Error where
  invalid = InvalidError

class AsUnset e where
  unset :: e

class AsEmpty e where
  empty :: e

class AsInvalid e where
  invalid :: String -> e
