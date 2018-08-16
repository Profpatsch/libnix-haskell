{-|
Description : Common Types
Copyright   : Profpatsch, 2018
License     : GPL-3
Stability   : experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Foreign.Nix.Shellout.Types where

import Protolude
import Control.Error

-- | Calls a command that returns an error and the whole stderr on failure.
newtype NixAction e a = NixAction
  { unNixAction :: ExceptT (NixActionError e) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Combines the standard error of running a command with a more semantic
-- error type one should match on first.
data NixActionError e = NixActionError
  { actionStderr :: Text
  , actionError :: e }
  deriving (Show, Functor)

-- | Run a 'NixAction' without having to go through 'ExceptT' first.
runNixAction :: NixAction e a
             -> IO (Either (NixActionError e) a)
runNixAction = runExceptT . unNixAction

instance Bifunctor NixAction where
  bimap f g = NixAction . bimapExceptT (fmap f) g . unNixAction

-- | A path in the nix store. It carries a phantom @a@ to differentiate
-- between 'Derivation' files and 'Realized' paths.
newtype StorePath a = StorePath
  { unStorePath :: FilePath }
  deriving (Eq, Show)

-- | A nix derivation is a complete build instruction that can be realized.
data Derivation
-- | Once a derivation is realized, the finished output can be used.
data Realized
