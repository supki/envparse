{-# LANGUAGE Safe #-}
-- | This module provides 'Mon', the 'Alternative' functor
-- induced by an instance of the 'Monoid' typeclass
module Env.Mon where

import Control.Applicative (Applicative(..), Alternative(..))
import Data.Monoid (Monoid(..))


-- | The 'Alternative' functor induced by the 'Monoid'
newtype Mon m a = Mon
  { unMon :: m
  } deriving (Show, Eq)

instance Functor (Mon m) where
  fmap _ (Mon a) = Mon a

instance Monoid m => Applicative (Mon m) where
  pure _ = Mon mempty
  Mon x <*> Mon y = Mon (mappend x y)

instance Monoid m => Alternative (Mon m) where
  empty = Mon mempty
  Mon x <|> Mon y = Mon (mappend x y)
