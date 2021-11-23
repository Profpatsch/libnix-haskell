{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Foreign.Nix.Shellout.Helpers where

import Foreign.Nix.Shellout.Types ( NixActionError(..), RunOptions (logFn, executables), LogFn (LogFn), NixAction, Executables )
import qualified System.Process as P
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified System.IO as SIO

-- needed for ignoreSigPipe
-- needed for ignoreSigPipe
import GHC.IO.Exception (IOErrorType(..), IOException(..), ExitCode)
import Foreign.C.Error (Errno(Errno), ePIPE)
import Data.Text (Text)
import Control.Error (ExceptT, runExceptT)
import Control.Concurrent (MVar, newEmptyMVar, forkIO, takeMVar, putMVar, killThread)
import Control.DeepSeq (rnf)

import Control.Exception (SomeException, throwIO, onException, try, mask, handle, evaluate)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as Text
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Data.Function ((&))

-- | Something we can run
data Executable =
  ExeFromPathEnv Text
  -- ^ name of the executable, to be looked up in PATH
  | ExeFromFilePath FilePath
  -- ^ a file path to the executable (can be relative or absolute)

-- | Get an executable from the 'Executables' option (by its getter)
-- or if not set use the given 'Text' as the name of the excutable
-- to be looked up in @PATH@.
getExecOr :: Monad m => (Executables -> Maybe FilePath) -> Text ->  NixAction e m Executable
getExecOr getter exeName =
  let f = \case
        Nothing -> ExeFromPathEnv exeName
        Just fp -> ExeFromFilePath fp
  in asks (f . getter . executables)

-- | Read the output of a process into a NixAction.
-- | Keeps stderr if process returns a failure exit code.
-- | The text is decoded as @UTF-8@.
readProcess :: (MonadIO m)
            => ((Text, Text) -> ExitCode -> ExceptT e m a)
            -- ^ handle (stdout, stderr) depending on the return value
            -> Executable
            -- ^ executable to run
            -> [Text]
            -- ^ arguments
            -> NixAction e m a
readProcess with exec args = do
  let exec' = case exec of
        ExeFromPathEnv name -> name
        ExeFromFilePath fp -> fp & Text.pack
  -- log every call based on the LogFn the user passed
  (LogFn l) <- asks logFn
  lift $ l exec' args

  (exc, out, err) <- liftIO
    $ readCreateProcessWithExitCodeAndEncoding
        (P.proc (Text.unpack exec') (map Text.unpack args)) SIO.utf8 ""
  lift (runExceptT (with (out, err) exc)) >>= \case
    Left e ->
      throwError $ NixActionError
        { actionStderr = err
        , actionError = e }
    Right a -> pure a

-- Copied & modified from System.Process (process-1.6.4.0)

-- | like @readCreateProcessWithExitCodeAndEncoding, but uses
-- | Text instead of [Char] and lets the user specify an encoding
-- | for the handles.
readCreateProcessWithExitCodeAndEncoding
    :: P.CreateProcess
    -> SIO.TextEncoding            -- ^ encoding for handles
    -> Text                        -- ^ standard input
    -> IO (ExitCode, Text, Text)   -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCodeAndEncoding cp encoding input = do
    let cp_opts = cp
          { P.std_in  = P.CreatePipe
          , P.std_out = P.CreatePipe
          , P.std_err = P.CreatePipe }
    -- todo: this is not exposed by System.Process
    -- withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
    P.withCreateProcess cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        SIO.hSetEncoding outh encoding
        SIO.hSetEncoding errh encoding
        SIO.hSetEncoding inh encoding

        out <- TIO.hGetContents outh
        err <- TIO.hGetContents errh

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (evaluate $ rnf out) $ \waitOut ->
         withForkWait (evaluate $ rnf err) $ \waitErr -> do

          -- now write any input
          unless (T.null input) $
            ignoreSigPipe $ TIO.hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ SIO.hClose inh

          -- wait on the output
          waitOut
          waitErr

          -- TODO: isn’t this done by `withCreateProcess`?
          SIO.hClose outh
          SIO.hClose errh

        -- wait on the process
        ex <- P.waitForProcess ph

        return (ex, out, err)


-- Copied from System.Process (process-1.6.4.0)

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of
  IOError { ioe_type  = ResourceVanished
          , ioe_errno = Just ioe }
    | Errno ioe == ePIPE -> return ()
  _ -> throwIO e
