{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Foreign.Nix.Shellout.Prefetch
( PrefetchError(..)
, runNixAction, NixAction(..)
, url, UrlOptions(..)
, Url(..), Sha256(..)
) where

import Protolude hiding (isPrefixOf)
import Control.Error hiding (bool, err)
import Data.Text (stripEnd, lines, isPrefixOf)

import Foreign.Nix.Shellout.Types
import qualified Foreign.Nix.Shellout.Helpers as Helpers

data PrefetchError
  = PrefetchOutputMalformed Text
  | ExpectedHashError
  | UnknownPrefetchError
  deriving (Eq, Show)

newtype Url = Url { unUrl :: Text } deriving (Show, Eq, IsString)
newtype Sha256 = Sha256 { unSha256 :: Text } deriving (Show, Eq, IsString)

data UrlOptions = UrlOptions
  { urlUrl :: Url
    -- ^ the URL
  , urlUnpack :: Bool
    -- ^ whether to unpack before hashing (useful for prefetching @fetchTarball@)
  , urlName :: Maybe Text
    -- ^ name of the store path
  , urlExpectedHash :: Maybe Sha256
    -- ^ the hash we are expecting
  }


-- | Runs @nix-prefetch-url@.
url :: UrlOptions -> NixAction PrefetchError (Sha256, StorePath Realized)
url UrlOptions{..} = Helpers.readProcess handler exec args
  where
    exec = "nix-prefetch-url"
    args =  bool [] ["--unpack"] urlUnpack
         <> maybe [] (\n -> ["--name", n]) urlName
         <> [ "--type", "sha256"
            , "--print-path"
            , unUrl urlUrl ]
         <> maybe [] (pure.unSha256) urlExpectedHash
    handler (out, err) = \case
      ExitSuccess -> withExceptT PrefetchOutputMalformed $ do
        let ls = lines $ stripEnd out
        path <- tryLast (exec <> " didn’t output a store path") ls
        sha  <- let errS = (exec <> " didn’t output a hash")
                in tryLast errS =<< tryInit errS ls
        pure (Sha256 sha, StorePath $ toS path)
      ExitFailure _ -> throwE $
        if "error: hash mismatch" `isPrefixOf` err
        then ExpectedHashError
        else UnknownPrefetchError
