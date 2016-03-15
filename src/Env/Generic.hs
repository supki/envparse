{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Env.Generic
  ( Record(..)
  , Field(..)
  , (?)(..)
  , G.Generic
  ) where

import           Control.Applicative (liftA2, (<|>))
import           Control.Monad (guard)
import qualified Data.Char as Char
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy(Proxy))
import qualified GHC.Generics as G
import qualified GHC.TypeLits as G
import           Numeric.Natural (Natural)
import           Prelude hiding (mod)

import qualified Env


class Record e a | a -> e where
  record :: Env.Parser e a
  default record :: (r ~ G.Rep a, G.Generic a, GRecord e r) => Env.Parser e a
  record =
    fmap G.to (gr State {statePrefix="", stateCon="", stateVar=""})

-- | Generic parsing state.
data State = State
  { statePrefix :: String -- ^ All variables' names have this prefix.
  , stateCon    :: String -- ^ Constructor currently being processed.
  , stateVar    :: String -- ^ Variable name to use for the next component.
  } deriving (Show, Eq)

class GRecord e f where
  gr :: State -> Env.Parser e (f a)

-- | We are not interested in any metadata of the type constructor definition.
instance GRecord e a => GRecord e (G.D1 c a) where
  gr =
    fmap G.M1 . gr

-- | Constant values are converted to 'Env.Parser's using their 'Field' instance.
instance (Env.AsUnset e, Field e a) => GRecord e (G.K1 i a) where
  gr State {stateVar} =
    fmap G.K1 (field stateVar Nothing)

-- | Constructor's name is used as a prefix to try to remove from
-- selectors when building environment variable names.
instance (G.Constructor c, GRecord e a) => GRecord e (G.C1 c a) where
  gr state =
    fmap G.M1 (gr state {stateCon=con})
   where
    con = G.conName (G.M1 Proxy :: G.M1 t c Proxy b)

-- | Products are converted to products of parsers.
instance (GRecord e f, GRecord e g) => GRecord e (f G.:*: g) where
  gr x =
    liftA2 (G.:*:) (gr x) (gr x)

-- | Sums are converted to sums of parsers.
instance (GRecord e f, GRecord e g) => GRecord e (f G.:+: g) where
  gr x =
    fmap G.L1 (gr x) <|> fmap G.R1 (gr x)

-- | Record selectors' names determine suffixes of environment variables' names.
instance (G.Selector c, Type c ~ 'Record, GRecord e a) => GRecord e (G.S1 c a) where
  gr state@State {statePrefix, stateCon} =
    fmap G.M1 (gr state {stateVar=statePrefix ++ suffix})
   where
    sel = G.selName (G.M1 Proxy :: G.M1 t c Proxy b)
    suffix = let
        x = camelTo2 sel
      in fromMaybe x $ do
        y <- List.stripPrefix (map Char.toLower stateCon) sel
        camelTo2 y <$ guard (not (List.null y))

-- | Stolen from Aeson and adapted.
camelTo2 :: String -> String
camelTo2 = map Char.toUpper . go2 . go1
 where
  go1 "" = ""
  go1 (x:u:l:xs) | Char.isUpper u && Char.isLower l = x : '_' : u : l : go1 xs
  go1 (x:xs) = x : go1 xs

  go2 "" = ""
  go2 (l:u:xs) | Char.isLower l && Char.isUpper u = l : '_' : u : go2 xs
  go2 (x:xs) = x : go2 xs

-- | Decide whether the constructor is a record.
type family Type x :: ConType where
  Type G.NoSelector = 'Plain
  Type x = 'Record

-- | Constructor can be either a plain thing or a record.
data ConType = Plain | Record

class Field e a where
  field :: String -> Maybe String -> Env.Parser e a
  default field :: (Env.AsUnset e, Env.AsUnread e, Read a) => String -> Maybe String -> Env.Parser e a
  field name help =
    Env.var Env.auto name (foldMap Env.help help)

instance (Env.AsUnset e, Env.AsUnread e) => Field e Int

instance (Env.AsUnset e, Env.AsUnread e) => Field e Int8

instance (Env.AsUnset e, Env.AsUnread e) => Field e Int16

instance (Env.AsUnset e, Env.AsUnread e) => Field e Int32

instance (Env.AsUnset e, Env.AsUnread e) => Field e Int64

instance (Env.AsUnset e, Env.AsUnread e) => Field e Integer

instance (Env.AsUnset e, Env.AsUnread e) => Field e Word

instance (Env.AsUnset e, Env.AsUnread e) => Field e Word8

instance (Env.AsUnset e, Env.AsUnread e) => Field e Word16

instance (Env.AsUnset e, Env.AsUnread e) => Field e Word32

instance (Env.AsUnset e, Env.AsUnread e) => Field e Word64

instance (Env.AsUnset e, Env.AsUnread e) => Field e Natural

instance (Env.AsUnset e, Env.AsUnread e) => Field e Float

instance (Env.AsUnset e, Env.AsUnread e) => Field e Double

instance Env.AsUnset e => Field e String where
  field name help =
    Env.var Env.str name (foldMap Env.help help)

instance (Env.AsUnset e, Env.AsUnread e) => Field e Char where
  field name help =
    Env.var reader name (foldMap Env.help help)
   where
    reader = \case
      [c] -> pure c
      str -> Left (Env.unread str)

instance (Env.AsUnset e, Env.AsEmpty e) => Field e Bool where
  field name help =
    Env.switch name (foldMap Env.help help)

-- | Variable tagged with its help message.
newtype a ? tag = Help { unHelp :: a }
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance (G.KnownSymbol tag, Field e a) => Field e (a ? tag) where
  field name _ =
    fmap Help (field name (pure (G.symbolVal (Proxy :: Proxy tag))))
