{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RecordShellToMarkdown.Data
  ( ShellIO(..)
  , RecordShellToMarkdownState
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import RecordShellToMarkdown.Print (joinSequence)

-- | Type Alias
--
-- This contains the entire shell session
type RecordShellToMarkdownState a = StateT (Seq ShellIO) IO a

-- | Shell Input and Output
data ShellIO = ShellIO
  { shell_inputs :: Seq T.Text -- ^ commands. If a command doesn't have stdout, it will be combined. For example, cd dir doesn't have an output so the next command will be concatenated to the command
  , shell_output :: !T.Text -- ^ output.
  , shell_wd :: T.Text -- ^ working directory when the output was printed
  }

instance Show ShellIO where
  show ShellIO {..} =
    T.unpack $
    joinSequence (fmap ("> " <>) shell_inputs) <> "\n" <> shell_output
