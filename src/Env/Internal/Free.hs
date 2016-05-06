{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @Alt F@ is the free 'Alternative' functor on @F@
module Env.Internal.Free
  ( Alt(..)
  , liftAlt
  , runAlt
  , foldAlt
  , hoistAlt
  -- * Debug
  , inspect
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif
import Control.Applicative (Alternative(..))
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..))
#endif


data Alt f a where
  Nope :: Alt f a
  Pure :: a -> Alt f a
  Ap   :: Alt f (a -> b) -> Alt f a -> Alt f b
  Alt  :: Alt f a -> Alt f a -> Alt f a
  Lift :: f a -> Alt f a

-- | Print the free structure
inspect :: Alt f a -> String
inspect Nope      = "Nope"
inspect (Pure _)  = "Pure _"
inspect (Ap f x)  = concat ["(", inspect f, ") <*> (", inspect x, ")"]
inspect (Alt x y) = concat ["(", inspect x, ") <|> (", inspect y, ")"]
inspect (Lift _)  = "Lift _"

instance Functor f => Functor (Alt f) where
  fmap _ Nope      = Nope
  fmap f (Pure a)  = Pure (f a)
  fmap f (Ap a v)  = Ap (fmap (f .) a) v
  fmap f (Alt a b) = Alt (fmap f a) (fmap f b)
  fmap f (Lift a)  = Lift (fmap f a)

instance Functor f => Applicative (Alt f) where
  pure = Pure
  (<*>) = Ap

instance Functor f => Alternative (Alt f) where
  empty = Nope
  (<|>) = Alt


liftAlt :: f a -> Alt f a
liftAlt = Lift

runAlt :: forall f g a. Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt u = go where
  go  :: Alt f b -> g b
  go Nope      = empty
  go (Pure a)  = pure a
  go (Ap f x)  = go f <*> go x
  go (Alt s t) = go s <|> go t
  go (Lift x)  = u x

foldAlt :: Monoid p => (forall a. f a -> p) -> Alt f b -> p
foldAlt f =
  unMon . runAlt (Mon . f)

hoistAlt :: forall f g b. Functor g => (forall a. f a -> g a) -> Alt f b -> Alt g b
hoistAlt nat =
  runAlt (Lift . nat)


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
