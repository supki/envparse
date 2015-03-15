{-# LANGUAGE ExtendedDefaultRules #-}
module EnvSpec (spec) where

import Control.Applicative
import Control.Monad
import Prelude hiding (pi)
import Test.Hspec
import Text.Read (readMaybe)

import Env

default (Integer, Double, String)


spec :: Spec
spec =
  describe "parsing" $ do
    it "parsing the environment with the noop parser always fails" $
      p empty `shouldBe` Nothing

    it "looking for the non-existing env var fails" $
      p (var str "xyzzy" mempty) `shouldBe` Nothing

    it "looking for the existing env var is a success" $
      p (var str "foo" mempty) `shouldBe` Just "bar"

    it "looking for many existing env vars is a success" $ do
      let x = (,) <$> var str "foo" mempty <*> var str "qux" mempty
      p x `shouldBe` Just ("bar", "quux")

    it "looking for the existing env var selects the active flag value" $
      p (flag 4 7 "foo" mempty) `shouldBe` Just 7

    it "looking for the existing but empty env var selects the default flag value" $
      p (flag 4 7 "empty" mempty) `shouldBe` Just 4

    it "looking for the non-existing env var selects the default flag value" $
      p (flag 4 7 "xyzzy" mempty) `shouldBe` Just 4

    context "readers" $ do
      it "can use a reader to parse the env var value" $
        p (var auto "num" mempty) `shouldBe` Just 4

      it "can use a custom reader" $ do
        p (var greaterThan5 "num"  mempty) `shouldBe` Nothing
        p (var greaterThan5 "num2" mempty) `shouldBe` Just 7

      it "'nonempty' weeds out variables set to the empty string" $
        p (var (str <=< nonempty) "empty" mempty) `shouldBe` Nothing

    context "alternatives" $ do
      it "can look through a list of alternatives" $
        p (asum
          [ var (\_ -> pure 1) "nope"       mempty
          , var (\_ -> pure 2) "still-nope" mempty
          , var (\_ -> pure 3) "yep"        mempty
          ]) `shouldBe` Just 3

      it "variables can have default values" $
        p (asum
          [ var (\_ -> pure 1) "nope"       mempty
          , var (\_ -> pure 2) "still-nope" (def 4)
          , var (\_ -> pure 3) "yep"        mempty
          ]) `shouldBe` Just 4

    context "modifiers" $ do
      it "the latter modifier overwrites the former" $
        p (var (\_ -> Left "nope") "never" (def 4 <> def 7)) `shouldBe` Just 7

      it "‘prefixed’ modifier changes the names of the variables" $
        pi (prefixed "spec_") (var str "foo" mempty) `shouldBe` Just "totally-not-bar"


greaterThan5 :: Reader Int
greaterThan5 s = note "fail" (do v <- readMaybe s; guard (v > 5); return v)

p :: Parser a -> Maybe a
p = pi mempty

pi :: Mod Info a -> Parser a -> Maybe a
pi m x = parseTest m x fancyEnv

fancyEnv :: [(String, String)]
fancyEnv =
  [ "foo"      ~> "bar"
  , "spec_foo" ~> "totally-not-bar"
  , "qux"      ~> "quux"
  , "num"      ~> "4"
  , "num2"     ~> "7"
  , "yep"      ~> "!"
  , "empty"    ~> ""
  ]
 where
  (~>) = (,)

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
