{-# LANGUAGE OverloadedStrings #-}
module Env.Internal.ParserSpec (spec) where

import Control.Monad ((<=<))
import Data.Text (Text)
import Test.Hspec

import Env.Internal.Error (Error, unread)
import Env.Internal.Parser


spec :: Spec
spec = do
  describe "str" $ do
    it "works with String" $
      str "foo" `shouldBe` ok ("foo" :: String)

    it "works with Text" $
      str "foo" `shouldBe` ok ("foo" :: Text)

  describe "splitOn" $ do
    it "splits the string on a separator character" $
      splitOn ',' "1,2,3" `shouldBe` ok ["1", "2", "3"]

    it "splits the empty string into the empty list" $
      splitOn ',' "" `shouldBe` ok []

    it "creates an extra element for leading and trailing separators" $
      splitOn ',' ",2," `shouldBe` ok ["", "2", ""]

    it "is nicely composable " $
      (mapM auto <=< splitOn ',') "1,2,3" `shouldBe` ok ([1, 2, 3] :: [Int])

  describe "char" $ do
    it "one character" $
      char "f" `shouldBe` ok 'f'

    it "empty" $
      char "" `shouldBe` Left (unread "must be a one-character string" :: Error)

    it "many characters" $
      char "foo" `shouldBe` Left (unread "must be a one-character string" :: Error)

ok :: a -> Either Error a
ok = Right
