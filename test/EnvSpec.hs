{-# LANGUAGE ExtendedDefaultRules #-}
module EnvSpec (spec) where

import Control.Applicative
import Control.Monad
import Test.Hspec
import Text.Read (readMaybe)

import Env

default (Integer, Double, String)


spec :: Spec
spec =
  describe "fromList" $ do
    it "parsing the environment with the noop parser always fails" $
      p empty `shouldBe` Nothing

    it "looking for the non-existing env var fails" $
      p (var "xyzzy" str) `shouldBe` Nothing

    it "looking for the existing env var is a success" $
      p (var "foo" str) `shouldBe` Just "bar"

    it "looking for many existing env vars is a success" $ do
      let x = (,) <$> var "foo" str <*> var "qux" str
      p x `shouldBe` Just ("bar", "quux")

    it "can use a reader to parse the env var value" $
      p (var "num" auto) `shouldBe` Just 4

    it "can use a custom reader" $ do
      p (var "num"  (reader greaterThan5)) `shouldBe` Nothing
      p (var "num2" (reader greaterThan5)) `shouldBe` Just 7

    it "can look through a list of alternatives" $
      p (asum
        [ var "nope"       (reader (\_ -> pure 1))
        , var "still-nope" (reader (\_ -> pure 2))
        , var "yep"        (reader (\_ -> pure 3))
        ]) `shouldBe` Just 3

    it "variables can have default values" $
      p (asum
        [ var "nope"       (reader (\_ -> pure 1))
        , var "still-nope" (reader (\_ -> pure 2) <> def 4)
        , var "yep"        (reader (\_ -> pure 3))
        ]) `shouldBe` Just 4

    it "the latter modifier overwrites the former" $
      p (var "nope" (def 4 <> def 7)) `shouldBe` Just 7

greaterThan5 :: Reader Int
greaterThan5 s = do v <- readMaybe s; guard (v > 5); return v

p :: Parser a -> Maybe a
p x = fromEnv x fancyEnv

fancyEnv :: [(String, String)]
fancyEnv =
  [ "foo"  ~> "bar"
  , "qux"  ~> "quux"
  , "num"  ~> "4"
  , "num2" ~> "7"
  , "yep"  ~> "!"
  ]
 where
  (~>) = (,)

