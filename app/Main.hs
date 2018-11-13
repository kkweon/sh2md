module Main where

import RecordShellToMarkdown (startShell)
import RecordShellToMarkdown.CLI (runCLIParser)

main :: IO ()
main = do
  parserInfo <- runCLIParser
  startShell parserInfo
