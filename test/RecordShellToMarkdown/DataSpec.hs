{-# LANGUAGE OverloadedStrings #-}

module RecordShellToMarkdown.DataSpec
  ( spec
  ) where

import Data.Sequence (fromList)
import Test.Hspec

import RecordShellToMarkdown.Data

spec :: Spec
spec =
  describe "show ShellIO" $
  it "returns a correct format" $
  let shellIO =
        ShellIO
          { shell_inputs = fromList ["cd app", "pwd"]
          , shell_output = "/home/kkweon"
          , shell_wd = "/home/kkweon"
          }
   in show shellIO `shouldBe` "> cd app\n> pwd\n/home/kkweon"
