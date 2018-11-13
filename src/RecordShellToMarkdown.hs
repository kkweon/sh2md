{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : RecordShellToMarkdown
Description : Create an interactive shell and record the inputs & outputs. When an user interrupts, print the result in markdown format
Copyright   : (c) Mo Kweon
License     : MIT
Maintainer  : kkweon@gmail.com
Stability   : experimental
Portability : POSIX

This module exports 'startShell' which starts an interactive shell

> startShell

CTRL + C if you want to stop recording

Then it will copy the result to the system clipboard.
-}
module RecordShellToMarkdown
  ( startShell
  , runShellCommand
  , runCommand
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (execStateT, get, modify, put)
import Data.Sequence (Seq((:|>), Empty))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (BufferMode(NoBuffering), hGetContents, hSetBuffering)
import System.Hclip (setClipboard)
import System.IO (stdout)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)
import qualified System.Process as Process

import qualified RecordShellToMarkdown.CLI as CLI
import RecordShellToMarkdown.Data (RecordShellToMarkdownState, ShellIO(..))
import RecordShellToMarkdown.Print (joinSequence)

-- | Start an Interactive shell
--
-- When an user interrupt __CTRL+C__ ('sigINT') then it will stop the session and print markdown
startShell :: CLI.CLIOption -> IO ()
startShell CLI.CLIOption {cli_stdout} = do
  hSetBuffering stdout NoBuffering
  mVar <- newMVar False
  _ <- installHandler sigINT (Catch $ handleInterrupt mVar) Nothing
  shellIOs <- execStateT (foreverRunShellCommand mVar) Empty
  let result = Seq.foldMapWithIndex (\_ x -> buildMarkdown x) shellIOs
  if cli_stdout
    then TIO.putStrLn result
    else setClipboard (T.unpack result) >>
         TIO.putStrLn "The output is copied to your clipboard"
  where
    printAsMarkdown :: ShellIO -> IO ()
    printAsMarkdown = TIO.putStrLn . buildMarkdown
    buildMarkdown :: ShellIO -> T.Text
    buildMarkdown ShellIO {..} =
      let inputs = "```\n" <> T.strip (joinSequence shell_inputs) <> "\n```\n"
       in if T.null shell_output
            then inputs
            else inputs <> "```\n" <> T.strip shell_output <> "\n```\n"

-- | Handle CTRL-C Interrupt
handleInterrupt :: MVar Bool -> IO ()
handleInterrupt mVar = do
  putStrLn "\nPress any key to complete"
  modifyMVar_ mVar (\_ -> return True)

-- | Run shell until CTRL-C is handled
foreverRunShellCommand :: MVar Bool -> RecordShellToMarkdownState ()
foreverRunShellCommand mVar = do
  liftIO $ TIO.putStr "$ "
  cmd <- T.pack <$> liftIO getLine
  unless (T.null cmd) $ runShellCommand cmd
  interrupted <- liftIO $ readMVar mVar
  unless interrupted $ foreverRunShellCommand mVar

-- | Run Shell Command
-- This is a wrapper 'runCommand'
runShellCommand :: T.Text -> RecordShellToMarkdownState ()
runShellCommand cmd = do
  stack <- get
  shellIO <-
    case stack of
      Empty -> runCommand cmd Nothing
      (_ :|> x) -> runCommand cmd (Just . shell_wd $ x)
  unless (T.null . shell_output $ shellIO) $
    liftIO . TIO.putStrLn . T.strip $ shell_output shellIO

-- | Run a shell command
--
-- If the current working directory(CWD) is given, then run the command in the CWD
runCommand ::
     T.Text -- ^ Shell command
  -> Maybe T.Text -- ^ Current working directory
  -> RecordShellToMarkdownState ShellIO
runCommand cmd cwd = do
  (_, maybeHout, _, _) <-
    liftIO $
    Process.createProcess
      (Process.shell (T.unpack cmd ++ ";pwd"))
        { Process.std_out = Process.CreatePipe
        , Process.new_session = False
        , Process.cwd = T.unpack <$> cwd
        }
  case maybeHout of
    Just hout -> do
      content <- liftIO . (T.lines . T.pack <$>) $ hGetContents hout
      handleContent cmd content
    Nothing -> fail "Failed to create a stdout handle"

handleContent :: T.Text -> [T.Text] -> RecordShellToMarkdownState ShellIO
handleContent cmd content = do
  shellIOs <- get
  case shellIOs of
    (rest :|> lastShellIO@ShellIO {..}) ->
      if T.null shell_output
        then let newShellIO =
                   lastShellIO
                     { shell_inputs = shell_inputs :|> cmd
                     , shell_output = shell_output <> T.unlines (init content)
                     , shell_wd = last content
                     }
              in put (rest :|> newShellIO) >> return newShellIO
        else addNewShellIO cmd content
    _ -> addNewShellIO cmd content

-- | Create a new ShellIO and add to the state
addNewShellIO :: T.Text -> [T.Text] -> RecordShellToMarkdownState ShellIO
addNewShellIO cmd content = do
  let shellIO =
        ShellIO (Seq.singleton cmd) (T.unlines (init content)) (last content)
  modify (:|> shellIO)
  return shellIO
