module RecordShellToMarkdown.CLI where

import Options.Applicative

newtype CLIOption = CLIOption
  { cli_stdout :: Bool
  } deriving (Show)

cliOptionParser :: Parser CLIOption
cliOptionParser =
  CLIOption <$>
  switch
    (long "stdout" <>
     help "Instead of copying to the clipboard, print the result to stdout")

cliOptionParserInfo :: ParserInfo CLIOption
cliOptionParserInfo =
  info
    (cliOptionParser <**> helper)
    (fullDesc <> progDesc "Record shell and print in markdown")

runCLIParser :: IO CLIOption
runCLIParser = execParser cliOptionParserInfo
