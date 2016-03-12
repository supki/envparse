module Env.Error
  ( Error(..)
  , AsUnset(..)
  , AsEmpty(..)
  , AsUnread(..)
  ) where


data Error
  = UnsetError
  | EmptyError
  | UnreadError String
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

class AsUnread e where
  unread :: String -> e
  tryUnread :: e -> Maybe String

instance AsUnread Error where
  unread = UnreadError
  tryUnread err =
    case err of
      UnreadError msg -> Just msg
      _ -> Nothing
