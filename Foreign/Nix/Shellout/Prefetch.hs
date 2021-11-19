{-|
Description : Wrapper for the @nix-prefetch@ CLI utilities
Copyright   : Profpatsch, 2018
License     : GPL-3
Stability   : experimental
Portability : nix-prefetch-scripts 2018 (no version number)

Calls to the @nix-prefetch-X@ utilities, to parse their output
into nice reusable data types.
-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Foreign.Nix.Shellout.Prefetch
( -- * nix-prefetch-url
  url, UrlOptions(..), defaultUrlOptions
  -- * nix-prefetch-git
, git, GitOptions(..), defaultGitOptions, GitOutput(..)
  -- * Types
, PrefetchError(..)
, Url(..), Sha256(..)
, module Foreign.Nix.Shellout.Types
) where

import Control.Error hiding (bool, err)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT

import Foreign.Nix.Shellout.Types
import qualified Foreign.Nix.Shellout.Helpers as Helpers
import Data.Text (Text)
import Data.String (IsString)
import GHC.IO.Exception (ExitCode(ExitFailure, ExitSuccess))
import qualified Data.Text as Text
import Data.Bool (bool)
import Data.Bifunctor (first)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.List as List
import Control.Monad.IO.Class (MonadIO)

data PrefetchError
  = PrefetchOutputMalformed Text
    -- ^ the tool’s output could not be parsed as expected
  | ExpectedHashError
    -- ^ an expected hash was given and not valid
  | UnknownPrefetchError
    -- ^ catch-all error
  deriving (Eq, Show)

-- | A descriptive type for URLs.
newtype Url = Url { unUrl :: Text } deriving (Show, Eq, IsString)
-- | A @sha-256@ hash.
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

-- | Takes the URL, doesn’t unpack and uses the default name.
defaultUrlOptions :: Url -> UrlOptions
defaultUrlOptions u = UrlOptions
  { urlUrl = u
  , urlUnpack = False
  , urlName = Nothing
  , urlExpectedHash = Nothing }

-- | Runs @nix-prefetch-url@.
url :: (MonadIO m) => UrlOptions -> NixAction PrefetchError m (Sha256, StorePath Realized)
url UrlOptions{..} = Helpers.readProcess handler exec args
  where
    exec = "nix-prefetch-url"
    args = if urlUnpack then ["--unpack"] else []
         <> maybe [] (\n -> ["--name", n]) urlName
         <> [ "--type", "sha256"
            , "--print-path"
            , unUrl urlUrl ]
         <> maybe [] (pure.unSha256) urlExpectedHash

    handler (out, err) = \case
      ExitSuccess -> withExceptT PrefetchOutputMalformed $ do
        let ls = T.lines $ T.stripEnd out
        path <- tryLast (exec <> " didn’t output a store path") ls
        sha  <- let errS = (exec <> " didn’t output a hash")
                in tryInit errS ls >>= tryLast errS
        pure (Sha256 sha, StorePath $ Text.unpack path)
      ExitFailure _ -> throwE $
        if "error: hash mismatch" `T.isPrefixOf` err
        then ExpectedHashError
        else UnknownPrefetchError


data GitOptions = GitOptions
  { gitUrl :: Url
    -- ^ the URL
  , gitRev :: Maybe Text
    -- ^ a git revision (hash, branch name, tag, ref, …)
  , gitExpectedHash :: Maybe Sha256
    -- ^ the hash we are expecting
  , gitDeepClone :: Bool
    -- ^ whether to do a deep instead of a shallow (@--depth=1@) git clone
  , gitLeaveDotGit :: Bool
    -- ^ whether to keep @.git@ directories
  , gitFetchSubmodules :: Bool
    -- ^ whether to fetch submodules
  }

-- | Takes the url, mirrors the default `fetchgit` options in nixpkgs:
-- no deep clone, no @.git@, fetches submodules by default.
-- By default, the latest default @rev@ is used.
defaultGitOptions :: Url -> GitOptions
defaultGitOptions u = GitOptions
  { gitUrl = u
  , gitRev = Nothing
  , gitExpectedHash = Nothing
  , gitDeepClone = False
  , gitLeaveDotGit = False
  , gitFetchSubmodules = True }

data GitOutput = GitOutput
  { gitOutputRev :: Text
    -- ^ The actual revision that is used (useful if no 'gitRev' was given)
  , gitOutputSha256 :: Sha256
    -- ^ the hash
  , gitOuputPath :: StorePath Realized
    -- ^ the store path of the result
  } deriving (Show, Eq)

-- | Runs @nix-prefetch-git@.
git :: (MonadIO m) => GitOptions -> NixAction PrefetchError m GitOutput
git GitOptions{..} = Helpers.readProcess handler exec args
  where
    exec = "nix-prefetch-git"
    args =  bool ["--no-deepClone"] ["--deepClone"] gitDeepClone
         <> bool [] ["--leave-dotGit"] gitLeaveDotGit
         <> bool [] ["--fetch-submodules"] gitFetchSubmodules
         <> [ "--hash", "sha256" -- --hash is the type, not the thing
            -- we need @url [rev [hash]]@,
            -- otherwise we can’t expect a hash
            , unUrl gitUrl
            , fromMaybe "" gitRev ]
            -- hash comes last
         <> maybe [] (\(Sha256 h) -> [h]) gitExpectedHash

    handler (out, err) = \case
      ExitSuccess -> withExceptT PrefetchOutputMalformed $ do
        let error' msg = exec <> " " <> msg
            jsonError :: [Char] -> Text
            jsonError = \msg -> error' (T.intercalate "\n"
                      [ "parsing json output failed:"
                      , Text.pack msg
                      , "The output was:"
                      , out ])

        (gitOutputRev, gitOutputSha256)
          <- ExceptT . pure . first jsonError $ do
            val <- Aeson.eitherDecode' (Text.Lazy.Encoding.encodeUtf8 $ Text.Lazy.fromStrict out)
            flip AesonT.parseEither val
              $ Aeson.withObject "GitPrefetchOutput" $ \obj -> do
                    (,) <$> obj Aeson..: "rev"
                        <*> fmap Sha256 (obj Aeson..: "sha256")

        -- The path isn’t output in the json, but on stderr. :(
        -- So this is a bit more hacky than necessary.
        gitOuputPath <- case
          List.find ("path is /nix/store" `T.isPrefixOf`) (T.lines err)
          >>= T.stripPrefix "path is " of
          Nothing -> throwE
            $ error "could not find nix store output path on stderr"
          Just path -> pure $ StorePath $ Text.unpack path

        pure GitOutput{..}

      ExitFailure _ -> throwE $
        if ("hash mismatch for URL" `T.isInfixOf` err)
        then ExpectedHashError
        else UnknownPrefetchError
