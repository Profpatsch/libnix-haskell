{-|
Description : Common Types
Copyright   : Profpatsch, 2018–2021
License     : GPL-3
Stability   : experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Foreign.Nix.Shellout.Types (
  -- * Store paths
  StorePath(..),
  Derivation,
  Realized,
  -- * NixAction
  runNixAction,
  NixAction(..),
  RunOptions(..),
  defaultRunOptions,
  LogFn(..),
  NixActionError(..),
  mapActionError,

) where

import Control.Error ( bimapExceptT, runExceptT, ExceptT )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT, ReaderT), MonadReader )
import Data.Text (Text)
import Control.Monad.Trans ( lift, MonadTrans )
import Control.Monad.Except (MonadError)

-- | Options that modify how a 'NixAction' executes.
--
-- Might get more fields in the future, use 'defaultRunOptions'
-- to be backwards-compatbile.
newtype RunOptions m = RunOptions {
  logFn :: LogFn m
  -- ^ The command line logging function.
}

-- | Logging function to call before running a command.
-- This can be used to provide debugging output.
--
-- The first argument is the executable name,
-- the second argument is the list of arguments.
newtype LogFn m = LogFn (Text -> [Text] -> m ())

-- |
-- @
-- logFn = nothing is done/logged
-- @
defaultRunOptions :: Monad m => RunOptions m
defaultRunOptions = RunOptions {
  logFn = LogFn (\_prog _args -> pure ())
}

-- | Calls a command that returns an error and the whole stderr on failure.
newtype NixAction e m a = NixAction
  { unNixAction :: ReaderT (RunOptions m) (ExceptT (NixActionError e) m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError (NixActionError e), MonadReader (RunOptions m))

instance MonadTrans (NixAction e) where
  lift = NixAction . lift . lift

-- | Combines the standard error of running a command with a more semantic
-- error type one should match on first.
data NixActionError e = NixActionError
  { actionStderr :: Text
  , actionError :: e }
  deriving (Show, Functor)

-- | Run a 'NixAction', given runtime options. See 'defaultRunOptions'.
runNixAction :: RunOptions m
             -> NixAction e m a
             -> m (Either (NixActionError e) a)
runNixAction runOpts act = runExceptT $ runReaderT (unNixAction act) runOpts

-- | Map over the @e@ in a 'NixActionError'.
mapActionError :: Functor m => (a1 -> e) -> NixAction a1 m a2 -> NixAction e m a2
mapActionError f (NixAction act) = NixAction $ ReaderT $ \runOpts -> bimapExceptT (fmap f) id $ runReaderT act runOpts

-- | A path in the nix store. It carries a phantom @a@ to differentiate
-- between 'Derivation' files and 'Realized' paths.
newtype StorePath a = StorePath
  { unStorePath :: FilePath }
  deriving (Eq, Show)

-- | A nix derivation is a complete build instruction that can be realized.
data Derivation
-- | Once a derivation is realized, the finished output can be used.
data Realized
