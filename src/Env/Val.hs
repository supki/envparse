{-# LANGUAGE DeriveFunctor #-}
module Env.Val
  ( Val(..)
  , fromEither
  , toEither
  ) where

import Control.Applicative
import Data.Monoid (Monoid(..), (<>))


-- | An isomorphic to 'Either' type with the accumulating 'Applicative' instance
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

  Err _ <|> x = x
  x     <|> _ = x

fromEither :: Either e a -> Val e a
fromEither = either Err Ok

toEither :: Val e a -> Either e a
toEither x = case x of Err e -> Left e; Ok a -> Right a
