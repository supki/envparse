{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Env.Internal.Val
  ( Val(..)
  , fromEither
  , toEither
  ) where

import Control.Applicative
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..))
#endif
import Data.Monoid ((<>))


-- | A type isomorphic to 'Either' with the accumulating 'Applicative' instance.
data Val e a
  = Err e
  | Ok  a
    deriving (Functor, Show, Eq)

instance Monoid e => Applicative (Val e) where
  pure = Ok

  Err e <*> Err e' = Err (e <> e')
  Err e <*> _      = Err e
  _     <*> Err e' = Err e'
  Ok  f <*> Ok  a  = Ok (f a)

instance Monoid e => Alternative (Val e) where
  empty = Err mempty

  Err _ <|> Ok x = Ok x
  x     <|> _    = x

fromEither :: Either e a -> Val e a
fromEither =
  either Err Ok

toEither :: Val e a -> Either e a
toEither x =
  case x of Err e -> Left e; Ok a -> Right a
