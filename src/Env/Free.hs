{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @Alt F@ is the free 'Alternative' functor on @F@
--
-- This code is shamelessly stolen from the "free" package
module Env.Free
  ( Alt(..)
  , AltF(..)
  , liftAlt
  , runAlt
  ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)


newtype Alt f a = Alt { unAlt :: [AltF f a] }

instance Functor f => Functor (Alt f) where
  fmap f (Alt xs) = Alt (map (fmap f) xs)

instance Functor f => Applicative (Alt f) where
  pure a = Alt [pure a]

  Alt xs <*> ys = Alt (concatMap (\x -> unAlt (ap x ys)) xs)
   where
    ap (Pure f) x = fmap f x
    ap (Ap x f) y = Alt [Ap x (fmap flip f <*> y)]

instance Functor f => Alternative (Alt f) where
  empty = Alt []
  Alt xs <|> Alt ys = Alt (xs ++ ys)


data AltF f a where
  Ap   :: f a -> Alt f (a -> b) -> AltF f b
  Pure :: a -> AltF f a

instance Functor f => Functor (AltF f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x fx) = Ap x (fmap (f .) fx)

instance Functor f => Applicative (AltF f) where
  pure = Pure

  Pure f <*> x      = fmap f x
  f      <*> Pure x = fmap ($ x) f
  Ap x f <*> y      = Ap x (liftA2 flip f (Alt [y]))


liftAlt :: f a -> Alt f a
liftAlt fx = Alt [Ap fx (Alt [Pure id])]

runAlt :: forall f g a. Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt u = go where
  go  :: Alt f b -> g b
  go (Alt xs) = foldr (\r a -> go2 r <|> a) empty xs

  go2 :: AltF f b -> g b
  go2 (Pure a) = pure a
  go2 (Ap x f) = liftA2 (flip id) (u x) (go f)
