{-# LANGUAGE OverloadedStrings #-}

module RecordShellToMarkdown.Print
  ( joinSequence
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-- | Concat @Seq T.Text@ into 'T.Text'
joinSequence :: Seq T.Text -> T.Text
joinSequence = Seq.foldrWithIndex folder ""
  where
    folder :: Int -> T.Text -> T.Text -> T.Text
    folder _ value accumulator =
      if T.null value
        then accumulator
        else value <> "\n" <> accumulator
