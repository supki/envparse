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

class AsUnset e where
  unset :: e
  tryUnset :: e -> Maybe ()

instance AsUnset Error where
  unset = UnsetError
  tryUnset err =
    case err of
      UnsetError -> Just ()
      _ -> Nothing

class AsEmpty e where
  empty :: e
  tryEmpty :: e -> Maybe ()

instance AsEmpty Error where
  empty = EmptyError
  tryEmpty err =
    case err of
      EmptyError -> Just ()
      _ -> Nothing

class AsInvalid e where
  invalid :: String -> e
  tryInvalid :: e -> Maybe String

instance AsInvalid Error where
  invalid = InvalidError
  tryInvalid err =
    case err of
      InvalidError msg -> Just msg
      _ -> Nothing
