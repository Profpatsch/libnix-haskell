module Foreign.Nix.Shellout.Helpers where

import Protolude
import Foreign.Nix.Shellout.Types
import System.Process (readProcessWithExitCode)

-- | Read the output of a process into a NixAction.
-- | Keeps stderr if process returns a failure exit code.
readProcess :: ((Text, Text) -> ExitCode -> ExceptT e IO a)
            -- ^ handle (stdout, stderr) depending on the return value
            -> Text
            -- ^ name of executable
            -> [Text]
            -- ^ arguments
            -> NixAction e a
            -- ^ error: (stderr, errormsg), success: path
readProcess with exec args = NixAction $ do
  (exc, out, err) <- liftIO
    $ readProcessWithExitCode (toS exec) (map toS args) ""
  withExceptT (toS err,) $ with (toS out, toS err) exc
