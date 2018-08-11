{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.Nix.Shellout.Types where

import Protolude
import Control.Error

-- | Calls a command that returns an error and the whole stderr on failure.
newtype NixAction e a = NixAction
  { unNixAction :: ExceptT (Text, e) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runNixAction :: NixAction e a -> IO (Either (Text, e) a)
runNixAction = runExceptT . unNixAction

instance Bifunctor NixAction where
  bimap f g = NixAction . bimapExceptT (fmap f) g . unNixAction


newtype StorePath a = StorePath
  { fromStorePath :: FilePath }
  deriving (Eq, Show)

data Derivation
data Realized
