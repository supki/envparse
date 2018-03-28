-- | This module contains an extensible error infrastructure.
--
-- Each kind of errors gets a separate type class which encodes
-- a 'Prism' (roughly a getter and a constructor). The 'Reader's, then,
-- have the constraints for precisely the set of errors they can return.
module Env.Internal.Error
  ( Error(..)
  , AsUnset(..)
  , AsEmpty(..)
  , AsUnread(..)
  , defaultIfUnset
  ) where


-- | The type of errors returned by @envparse@'s 'Reader's. These fall into 3
-- categories:
--
--   * Variables that are unset in the environment.
--   * Variables whose value is empty.
--   * Variables whose value cannot be parsed using the 'Read' instance.
data Error
  = UnsetError
  | EmptyError
  | UnreadError String
    deriving (Show, Eq)

-- | The class of types that contain and can be constructed from
-- the error returned from parsing unset variables.
class AsUnset e where
  unset :: e
  tryUnset :: e -> Maybe ()

defaultIfUnset :: AsUnset e => a -> e -> Either e a
defaultIfUnset d e
  | tryUnset e == Just () = Right d
  | otherwise = Left e

instance AsUnset Error where
  unset = UnsetError
  tryUnset err =
    case err of
      UnsetError -> Just ()
      _ -> Nothing

-- | The class of types that contain and can be constructed from
-- the error returned from parsing variables whose value is empty.
class AsEmpty e where
  empty :: e
  tryEmpty :: e -> Maybe ()

instance AsEmpty Error where
  empty = EmptyError
  tryEmpty err =
    case err of
      EmptyError -> Just ()
      _ -> Nothing

-- | The class of types that contain and can be constructed from
-- the error returned from parsing variable whose value cannot
-- be parsed using the 'Read' instance.
class AsUnread e where
  unread :: String -> e
  tryUnread :: e -> Maybe String

instance AsUnread Error where
  unread = UnreadError
  tryUnread err =
    case err of
      UnreadError msg -> Just msg
      _ -> Nothing
