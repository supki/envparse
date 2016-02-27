module Env.Error
  ( Error(..)
  , Unset(..)
  , AsUnset(..)
  , Empty(..)
  , AsEmpty(..)
  , Invalid(..)
  , AsInvalid(..)
  ) where


data Error
  = UnsetError Unset
  | EmptyError Empty
  | InvalidError Invalid
    deriving (Show, Eq)

instance AsUnset Error where
  unset =
    UnsetError . Unset

instance AsEmpty Error where
  empty =
    EmptyError . Empty

instance AsInvalid Error where
  invalid val =
    InvalidError . Invalid val


newtype Unset = Unset { unUnset :: String }
    deriving (Show, Eq)

class AsUnset e where
  unset :: String -> e

instance AsUnset Unset where
  unset = Unset


newtype Empty = Empty { unEmpty :: String }
    deriving (Show, Eq)

class AsEmpty e where
  empty :: String -> e

instance AsEmpty Empty where
  empty = Empty


data Invalid
  = Invalid String String
    deriving (Show, Eq)

class AsInvalid e where
  invalid :: String -> String -> e

instance AsInvalid Invalid where
  invalid = Invalid
