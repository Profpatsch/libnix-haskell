{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ApplicativeDo #-}
module Foreign.Nix.Shellout.Prefetch
( PrefetchError(..)
, runNixAction, NixAction(..)
, Url(..), Sha256(..)
, url, UrlOptions(..), defaultUrlOptions
, git, GitOptions(..), defaultGitOptions, GitOutput(..)
) where

import Protolude
import Control.Error hiding (bool, err)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT

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

-- | Takes the URL, doesn’t unpack and uses the default name.
defaultUrlOptions :: Url -> UrlOptions
defaultUrlOptions u = UrlOptions
  { urlUrl = u
  , urlUnpack = False
  , urlName = Nothing
  , urlExpectedHash = Nothing }

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
        let ls = T.lines $ T.stripEnd out
        path <- tryLast (exec <> " didn’t output a store path") ls
        sha  <- let errS = (exec <> " didn’t output a hash")
                in tryInit errS ls >>= tryLast errS
        pure (Sha256 sha, StorePath $ toS path)
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
-- | no deep clone, no @.git@, fetches submodules by default.
-- | By default, the latest default @rev@ is used.
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
  } deriving (Show, Eq)

git :: GitOptions -> NixAction PrefetchError GitOutput
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
            , maybe "" identity gitRev ]
            -- hash comes last
         <> maybe [] (\(Sha256 h) -> [h]) gitExpectedHash

    handler (out, err) = \case
      ExitSuccess -> withExceptT PrefetchOutputMalformed $ do
        let error msg = exec <> " " <> msg
            jsonError :: [Char] -> Text
            jsonError = \msg -> error (T.intercalate "\n"
                      [ "parsing json output failed:"
                      , toS msg
                      , "The output was:"
                      , out ])

        (gitOutputRev, gitOutputSha256)
          <- ExceptT . pure . first jsonError $ do
            val <- Aeson.eitherDecode' (toS out)
            flip AesonT.parseEither val
              $ Aeson.withObject "GitPrefetchOutput" $ \obj -> do
                    (,) <$> obj Aeson..: "rev"
                        <*> fmap Sha256 (obj Aeson..: "sha256")

        -- The path isn’t output in the json, but on stderr. :(
        -- So this is a bit more hacky than necessary.
        gitOuputPath <- case
          find ("path is /nix/store" `T.isPrefixOf`) (T.lines err)
          >>= T.stripPrefix "path is " of
          Nothing -> throwE
            $ error "could not find nix store output path on stderr"
          Just path -> pure $ StorePath $ toS path

        pure GitOutput{..}

      ExitFailure _ -> throwE $
        if ("hash mismatch for URL" `T.isInfixOf` err)
        then ExpectedHashError
        else UnknownPrefetchError
