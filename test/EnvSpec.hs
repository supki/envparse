{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module EnvSpec (spec) where

import           Control.Applicative
import           Control.Monad
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (mempty)
#endif
import           Prelude hiding (pi)
#if __GLASGOW_HASKELL__ >= 708
import           System.Environment (lookupEnv, setEnv)
#endif
import           Test.Hspec
import           Text.Read (readMaybe)

import           Env

default (Integer, Double, String)


spec :: Spec
spec =
  describe "parsing" $ do
    it "parsing the environment with the noop parser always succeeds" $
      p (pure ()) `shouldBe` Just ()

    it "parsing the environment with the failing parser always fails" $
      p Control.Applicative.empty `shouldBe` Nothing

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
        p (var (\_ -> Left (unread "nope")) "never" (def 4 <> def 7)) `shouldBe` Just 7

      it "‘prefixed’ modifier changes the names of the variables" $
        p (prefixed "spec_" (var str "foo" mempty)) `shouldBe` Just "totally-not-bar"

      it "‘prefixed’ modifier can be nested" $
        p (prefixed "pre" (prefixed "pro" (var str "morphism" mempty)))
       `shouldBe`
        Just "zygohistomorphic"

#if __GLASGOW_HASKELL__ >= 708
    it "unsets parsed variables" $ do
      setEnv "FOO" "4"
      setEnv "BAR" "7"
      parse (header "hi") (liftA2 (+) (var auto "FOO" (help "a")) (var auto "BAR" (help "b"))) `shouldReturn` (11 :: Int)
      lookupEnv "FOO" `shouldReturn` Nothing
      lookupEnv "BAR" `shouldReturn` Nothing

    context "some variables are marked as kept" $
      it "does not unset them" $ do
        setEnv "FOO" "4"
        setEnv "BAR" "7"
        parse (header "hi") (liftA2 (+) (var auto "FOO" (help "a" <> keep)) (var auto "BAR" (help "b"))) `shouldReturn` (11 :: Int)
        lookupEnv "FOO" `shouldReturn` Just "4"
        lookupEnv "BAR" `shouldReturn` Nothing

    context "parsing fails" $
      it "does not unset any variables" $ do
        setEnv "FOO" "4"
        setEnv "BAR" "bar"
        parse (header "hi") (liftA2 (+) (var auto "FOO" (help "a" <> keep)) (var auto "BAR" (help "b"))) `shouldThrow` anyException
        lookupEnv "FOO" `shouldReturn` Just "4"
        lookupEnv "BAR" `shouldReturn` Just "bar"
#endif


greaterThan5 :: AsUnread e => Reader e Int
greaterThan5 s =
  note (unread "fail") (do v <- readMaybe s; guard (v > 5); return v)

p :: Parser Error a -> Maybe a
p x = hush (parsePure x fancyEnv)

fancyEnv :: [(String, String)]
fancyEnv =
  [ "foo"      ~> "bar"
  , "spec_foo" ~> "totally-not-bar"
  , "prepromorphism"
               ~> "zygohistomorphic"
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

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
