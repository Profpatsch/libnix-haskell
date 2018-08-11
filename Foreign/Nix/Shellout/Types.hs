{-|
Description : Common Types
Copyright   : Profpatsch, 2018
License     : GPL-3
Stability   : experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.Nix.Shellout.Types where

import Protolude
import Control.Error

-- | Calls a command that returns an error and the whole stderr on failure.
newtype NixAction e a = NixAction
  { unNixAction :: ExceptT (Text, e) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a 'NixAction' without having to go through 'ExceptT' first.
runNixAction :: NixAction e a -> IO (Either (Text, e) a)
runNixAction = runExceptT . unNixAction

instance Bifunctor NixAction where
  bimap f g = NixAction . bimapExceptT (fmap f) g . unNixAction

-- | A path in the nix store. It carries a phantom @a@ to differentiate
-- between 'Derivation' files and 'Realized' paths.
newtype StorePath a = StorePath
  { fromStorePath :: FilePath }
  deriving (Eq, Show)

-- | A nix derivation is a complete build instruction that can be realized.
data Derivation
-- | Once a derivation is realized, the finished output can be used.
data Realized
