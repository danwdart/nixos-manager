{-|
  Description: Provides a thin layer above "System.Process" - there’s probably something nice out that that can be used instead.

Provides a thin layer above "System.Process" - there’s probably something nice out that that can be used instead.
  -}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.Process
  ( ProcessOutput
  , ProcessData
  , runProcess
  , runProcessToFinish
  , terminate
  , waitUntilFinished
  , getProcessId
  , updateProcess
  , noStdin
  )
where

import           Control.Lens         (view, (^.))
import           Data.ByteString      (ByteString, hGetContents,
                                       hGetNonBlocking, hPutStr)
import           Data.Foldable        (for_)
import           Data.Generics.Labels ()
import           Data.Monoid          (First (First))
import           Data.Text            (unpack)
import           Data.Text.IO         (putStrLn)
import           GHC.Generics         (Generic)
import           NixManager.Bash      (Expr (Command), argText, evalExpr)
import           Prelude              hiding (putStrLn)
import           System.Exit          (ExitCode)
import           System.IO            (Handle)
import           System.Process       (CmdSpec (RawCommand, ShellCommand),
                                       CreateProcess (..), Pid, ProcessHandle,
                                       StdStream (CreatePipe), createProcess,
                                       getPid, getProcessExitCode,
                                       terminateProcess, waitForProcess)

-- | Represents all the data needed to handle a running process
data ProcessData = ProcessData {
    stdoutHandle  :: Handle -- ^ The handle to stdout
  , stderrHandle  :: Handle -- ^ The handle to stderr
  , processHandle :: ProcessHandle -- ^ The process handle
  } deriving(Generic)

-- | Represents output from a process (either “in total” or partially)
data ProcessOutput = ProcessOutput {
    stdout :: ByteString -- ^ A piece of stdout output
  , stderr :: ByteString -- ^ A piece of stderr output
  , result :: First ExitCode -- ^ Optional exit code (type chosen so semigroup/monoid works)
  } deriving(Generic)

instance Semigroup ProcessOutput where
  (ProcessOutput a b c) <> (ProcessOutput a' b' c') =
    ProcessOutput (a <> a') (b <> b') (c <> c')

instance Monoid ProcessOutput where
  mempty = ProcessOutput mempty mempty mempty

-- | Terminate the process. In case you’re wondering why this isn’t actually used: I tried this on the sudo processes (like for rebuilding), and this terminate doesn’t throw an exception, /however/, it also doesn’t kill the process. This might just be my misunderstanding of Linux processes.
terminate :: ProcessData -> IO ()
terminate = terminateProcess . view #processHandle

-- | Convert a Bash expression (see the corresponding module) to a "System.Process" 'CmdSpec'
exprToCmdSpec :: Expr -> CmdSpec
exprToCmdSpec (Command x args) =
  RawCommand (unpack x) (unpack . argText <$> args)
exprToCmdSpec x = ShellCommand (unpack (evalExpr x))

-- | Signify “I don’t want to pass anything on stdin”. Yeah, I was too lazy for a separate data type here.
noStdin :: Maybe ByteString
noStdin = Nothing

-- | Get the processe’s ID (potentially unsafe, though I don’t know under what circumstances)
getProcessId :: ProcessData -> IO (Maybe Pid)
getProcessId = getPid . view #processHandle

-- | Start a process, wait for it to finish, and return its result.
runProcessToFinish :: Maybe ByteString -> Expr -> IO ProcessOutput
runProcessToFinish stdinString command = do
  pd <- runProcess stdinString command
  waitUntilFinished pd

-- | Start a process, potentially feeding a constant amount of data into stdin, and return the data to manage it further.
runProcess :: Maybe ByteString -> Expr -> IO ProcessData
runProcess stdinString command = do
  putStrLn ("Executing: " <> evalExpr command)
  (Just hin, Just hout, Just herr, ph) <- createProcess $ CreateProcess
    { cmdspec            = exprToCmdSpec command
    , cwd                = Nothing
    , env                = Nothing
    , std_in             = CreatePipe
    , std_out            = CreatePipe
    , std_err            = CreatePipe
    , close_fds          = False
    , create_group       = False
    , delegate_ctlc      = False
    , detach_console     = False
    , create_new_console = False
    , new_session        = False
    , child_group        = Nothing
    , child_user         = Nothing
    , use_process_jobs   = False
    }
  for_ stdinString (hPutStr hin)
  pure (ProcessData hout herr ph)

-- | Wait for the process to finish, return all its (remaining) data.
waitUntilFinished :: ProcessData -> IO ProcessOutput
waitUntilFinished pd = do
  stdoutContent <- hGetContents (pd ^. #stdoutHandle)
  stderrContent <- hGetContents (pd ^. #stderrHandle)
  exitCode      <- waitForProcess (pd ^. #processHandle)
  pure (ProcessOutput stdoutContent stderrContent (First (Just exitCode)))

-- | Get some data from the process (for potentially the last time).
updateProcess :: ProcessData -> IO ProcessOutput
updateProcess pd = do
  newStdout   <- hGetNonBlocking (pd ^. #stdoutHandle) 1024
  newStderr   <- hGetNonBlocking (pd ^. #stderrHandle) 1024
  newExitCode <- getProcessExitCode (pd ^. #processHandle)
  pure (ProcessOutput newStdout newStderr (First newExitCode))
