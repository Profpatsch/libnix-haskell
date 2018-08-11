{-# LANGUAGE RecordWildCards #-}
module Foreign.Nix.Shellout.Prefetch
( PrefetchError(..)
, runNixAction, NixAction(..)
, Sha256(..)
, url, UrlOptions(..)
) where

import Protolude
import Control.Error hiding (bool)
import Data.Text (stripEnd, lines)

import Foreign.Nix.Shellout.Types
import qualified Foreign.Nix.Shellout.Helpers as Helpers

data PrefetchError
  = PrefetchOutputMalformed Text
  | UnknownPrefetchError
  deriving (Eq, Show)

data UrlOptions = UrlOptions
  { urlUrl :: Text
  , urlUnpack :: Bool
  , urlName :: Maybe Text }

data Sha256 = Sha256 Text
  deriving (Show, Eq)

url :: UrlOptions -> NixAction PrefetchError (Sha256, StorePath Realized)
url UrlOptions{..} = Helpers.readProcess handler exec args
  where
    exec = "nix-prefetch-url"
    args =  bool [] ["--unpack"] urlUnpack
         <> maybe [] (\n -> ["--name", n]) urlName
         <> [ "--type", "sha256"
            , "--print-path"
            , urlUrl ]
    handler (out, _) = \case
      ExitSuccess -> withExceptT PrefetchOutputMalformed $ do
        let ls = lines $ stripEnd out
        path <- tryLast (exec <> " didn’t output a store path") ls
        sha  <- let errS = (exec <> " didn’t output a hash")
                in tryLast errS =<< tryInit errS ls
        pure (Sha256 sha, StorePath $ toS path)
      ExitFailure _ -> throwE UnknownPrefetchError
