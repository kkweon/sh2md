{-# LANGUAGE OverloadedStrings #-}

module RecordShellToMarkdown.PrintSpec
  ( spec
  ) where

import Data.Sequence
import Test.Hspec

import RecordShellToMarkdown.Print

spec :: Spec
spec =
  describe "joinSequence" $ do
    it "can join Seq T.Text" $
        joinSequence (fromList ["hello", "world"]) `shouldBe` "hello\nworld"

    it "can join Seq T.Text with empty text" $
        joinSequence (fromList ["Hello", "", "world"]) `shouldBe` "Hello\nworld"
