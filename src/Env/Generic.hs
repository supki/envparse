{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE ExplicitNamespaces #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Using the 'G.Generic' facility, this module can derive 'Env.Parser's automatically.
--
-- If you have a simple record:
--
-- @
-- {-\# LANGUAGE DeriveGeneric #-}
-- {-\# LANGUAGE MultiParamTypeClasses #-}
--
-- import Env
-- import Env.Generic
--
-- data Hello = Hello
--   { name  :: String
--   , count :: Int
--   , quiet :: Bool
--   } deriving (Show, Eq, Generic)
--
-- instance Record Error Hello
--
-- main :: IO ()
-- main = do
--   hello <- Env.parse (header "envparse example") 'record'
--   print (hello :: Hello)
-- @
--
-- The generic implementation of the 'record' method translates named fields to field parsers:
--
-- @
-- % NAME=bob COUNT=3 runhaskell -isrc example/Generic0.hs
-- Hello {name = "bob", count = 3, quiet = False}
-- @
--
-- If you want to adorn the ugly default help message, augment the fields with descriptions:
--
-- @
-- {-\# LANGUAGE DataKinds #-}
-- {-\# LANGUAGE DeriveGeneric #-}
-- {-\# LANGUAGE MultiParamTypeClasses #-}
-- {-\# LANGUAGE TypeOperators #-}
--
-- import Env
-- import Env.Generic
--
-- data Hello = Hello
--   { name  :: String ? __"Whom shoud I greet?"__
--   , count :: Int    ? __"How many times to greet them?"__
--   , quiet :: Bool   ? __"Should I be quiet instead?"__
--   } deriving (Show, Eq, Generic)
--
-- instance Record Error Hello
--
-- main :: IO ()
-- main = do
--   hello <- Env.parse (header "envparse example") record
--   print (hello :: Hello)
-- @
--
-- @
-- % runhaskell -isrc example/Generic1.hs
-- envparse example
--
-- Available environment variables:
--
--   COUNT                  __How many times to greet them?__
--   NAME                   __Whom shoud I greet?__
--   QUIET                  __Should I be quiet instead?__
--
-- Parsing errors:
--
--   COUNT is unset
--   NAME is unset
-- @
--
-- Note that this has an effect of wrapping the values in the 'Help' constructor:
--
-- @
-- % NAME=bob COUNT=3 QUIET='YES' runhaskell -isrc example/Generic1.hs
-- Hello {name = Help {unHelp = "bob"}, count = Help {unHelp = 3}, quiet = Help {unHelp = True}}
-- @
module Env.Generic
  ( Record(..)
  , Field(..)
#if __GLASGOW_HASKELL__ < 800
  , (?)(..)
#else
  , type (?)(..)
#endif
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


-- | Given a @Record e a@ instance, a value of the type @a@ can be parsed from the environment.
-- If the parsing fails, a value of an error type @e@ is returned.
--
-- The 'record' method has a default implementation for any type that has a 'G.Generic' instance. If you
-- need to choose a concrete type for @e@, the default error type 'Env.Error' is a good candidate. Otherwise,
-- the features you'll use in your parsers will naturally guide GHC to compute the set of required
-- constraints on @e@.
class Record e a where
  record :: Env.Parser e a
  default record :: (r ~ G.Rep a, G.Generic a, GRecord e r) => Env.Parser e a
  record =
    fmap G.to (gr State {statePrefix="", stateCon="", stateVar=""})

data State = State
  { statePrefix :: String -- All the variables' names have this prefix.
  , stateCon    :: String -- The constructor currently being processed.
  , stateVar    :: String -- The variable name to use for the next component.
  } deriving (Show, Eq)

class GRecord e f where
  gr :: State -> Env.Parser e (f a)

instance GRecord e a => GRecord e (G.D1 c a) where
  gr =
    fmap G.M1 . gr

-- Constant values are converted to 'Env.Parser's using their 'Field' instance.
instance Field e a => GRecord e (G.K1 i a) where
  gr State {stateVar} =
    fmap G.K1 (field stateVar Nothing)

-- Constructor's name is used as a prefix to try to remove from
-- selectors when building environment variable names.
instance (G.Constructor c, GRecord e a) => GRecord e (G.C1 c a) where
  gr state =
    fmap G.M1 (gr state {stateCon=con})
   where
    con = G.conName (G.M1 Proxy :: G.M1 t c Proxy b)

instance (GRecord e f, GRecord e g) => GRecord e (f G.:*: g) where
  gr x =
    liftA2 (G.:*:) (gr x) (gr x)

instance (GRecord e f, GRecord e g) => GRecord e (f G.:+: g) where
  gr x =
    fmap G.L1 (gr x) <|> fmap G.R1 (gr x)

#if __GLASGOW_HASKELL__ < 800
type family Type x :: ConType where
  Type G.NoSelector = 'Plain
  Type x = 'Record

data ConType = Plain | Record

instance (G.Selector c, Type c ~ 'Record, GRecord e a) => GRecord e (G.S1 c a) where
#else
instance (G.Selector c, c ~ 'G.MetaSel ('Just x1) x2 x3 x4, GRecord e a) => GRecord e (G.S1 c a) where
#endif
  gr state@State {statePrefix, stateCon} =
    fmap G.M1 (gr state {stateVar=statePrefix ++ suffix})
   where
    sel = G.selName (G.M1 Proxy :: G.M1 t c Proxy b)
    suffix = let
        x = camelTo2 sel
      in fromMaybe x $ do
        y <- List.stripPrefix (map Char.toLower stateCon) sel
        camelTo2 y <$ guard (not (List.null y))

camelTo2 :: String -> String
camelTo2 = map Char.toUpper . go2 . go1
 where
  go1 "" = ""
  go1 (x:u:l:xs) | Char.isUpper u && Char.isLower l = x : '_' : u : l : go1 xs
  go1 (x:xs) = x : go1 xs

  go2 "" = ""
  go2 (l:u:xs) | Char.isLower l && Char.isUpper u = l : '_' : u : go2 xs
  go2 (x:xs) = x : go2 xs

-- | Given a @Field e a@ instance, a value of the type @a@ can be parsed from an environment variable.
-- If the parsing fails, a value of an error type @e@ is returned.
--
-- The 'field' method has a default implementation for any type that has a 'Read' instance. If you
-- need to choose a concrete type for @e@, the default error type 'Env.Error' is a good candidate. Otherwise,
-- the features you'll use in your parsers will naturally guide GHC to compute the set of required
-- constraints on @e@.
--
-- The annotated instances do not use the default implementation.
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

-- | Optional parser.
instance (Field e a, Env.AsUnset e, Env.AsUnread e) => Field e (Maybe a) where
  field name help = Env.optional (field name help)

-- | Uses the 'String' value verbatim.
instance Env.AsUnset e => Field e String where
  field name help =
    Env.var Env.str name (foldMap Env.help help)

-- | Expects a single-character 'String' value.
instance (Env.AsUnset e, Env.AsUnread e) => Field e Char where
  field name help =
    Env.var reader name (foldMap Env.help help)
   where
    reader = \case
      [c] -> pure c
      str -> Left (Env.unread str)

-- | Any set and non-empty value parses to a 'True'; otherwise, it's a 'False'. This parser
-- never fails.
instance Field e Bool where
  field name help =
    Env.switch name (foldMap Env.help help)

-- | A field annotation.
--
-- If you annotate a record field with a 'Symbol' literal (that is, a statically known type level string)
-- the derivation machinery will use the literal in the help message.
--
-- Please remember that the values of the annotated fields are wrapped in the 'Help' constructor.
newtype a ? tag = Help { unHelp :: a }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Augments the underlying field parser with the help message.
instance (G.KnownSymbol tag, Field e a) => Field e (a ? tag) where
  field name _ =
    fmap Help (field name (pure (G.symbolVal (Proxy :: Proxy tag))))
