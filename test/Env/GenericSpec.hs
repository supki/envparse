{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Env.GenericSpec (spec) where

import Test.Hspec
#if __GLASGOW_HASKELL__ < 710


spec :: Spec
spec =
  return ()
#else

import Env
import Env.Generic


spec :: Spec
spec = do
  fooBarSpec
  quxSpec

data FooBar = FooBar
  { foo :: String
  , bar :: Int
  } deriving (Show, Eq, Generic)

instance Record Error FooBar

fooBarSpec :: Spec
fooBarSpec =
  describe "FooBar" $
    it "can be parsed using the Generic parser" $
      parsePure record [("FOO", "blah"), ("BAR", "4")] `shouldBe`
        pure FooBar {foo="blah", bar=4}

data Qux = Qux
  { quxXyz   :: Bool
  , quxXyzZy :: Int
  } deriving (Show, Eq, Generic)

instance Record Error Qux

quxSpec :: Spec
quxSpec =
  describe "Qux" $
    it "can be parsed using the Generic parser" $
      parsePure record [("XYZ", "1"), ("XYZ_ZY", "7")] `shouldBe`
        pure Qux {quxXyz=True, quxXyzZy=7}
#endif
