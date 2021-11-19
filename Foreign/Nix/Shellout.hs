{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Foreign.Nix.Shellout
Description : Interface to the nix package manager’s CLI
Copyright   : Profpatsch, 2016–2018
License     : GPL-3
Stability   : experimental
Portability : nix 1.11.x, nix 2.0

Calls to the nix command line to convert
textual nix expressions to derivations & realized storepaths.
-}
module Foreign.Nix.Shellout
( -- * Calling nix
  -- ** Parse
  parseNixExpr, ParseError(..)
  -- ** Instantiate
, instantiate, InstantiateError(..)
, eval
  -- ** Realize
, realize, RealizeError(..)
  -- ** Helpers
, addToStore
, parseInstRealize
, NixError(..)
  -- * Types
, NixExpr
, module Foreign.Nix.Shellout.Types
) where

import Control.Error ( throwE, tryLast )
import Data.Text (stripPrefix, lines, isPrefixOf, Text)

import qualified Foreign.Nix.Shellout.Helpers as Helpers
import Foreign.Nix.Shellout.Types
import qualified Data.Text as Text
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import Control.Monad ((>=>))
import qualified System.FilePath as FilePath
import Data.Function ((&))
import qualified Data.List as List
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (throwError)

------------------------------------------------------------------------------
-- Parsing

-- | A sucessfully parsed nix expression.
newtype NixExpr = NixExpr Text deriving (Show)

data ParseError
  = SyntaxError Text
    -- ^ the input string was not a syntactically valid nix expression
  | UnknownParseError
    -- ^ catch-all error
  deriving (Show, Eq)

-- | Parse a nix expression and check for syntactic validity.
parseNixExpr :: MonadIO m => Text -> NixAction ParseError m NixExpr
parseNixExpr e =
  mapActionError parseParseError
    $ NixExpr
    <$> evalNixOutput "nix-instantiate" [ "--parse", "-E", e ]


parseParseError :: Text -> ParseError
parseParseError
  (stripPrefix "error: syntax error, "
               -> Just mes) = SyntaxError $ mes
parseParseError _           = UnknownParseError

------------------------------------------------------------------------------
-- Instantiating

data InstantiateError
  = NotADerivation
    -- ^ the given expression does not evaluate to a derivaton
  | UnknownInstantiateError
    -- ^ catch-all error
  deriving (Show, Eq)

-- | Instantiate a parsed expression into a derivation.
instantiate :: (MonadIO m) => NixExpr -> NixAction InstantiateError m (StorePath Derivation)
instantiate (NixExpr e) =
  mapActionError parseInstantiateError
    $ evalNixOutput "nix-instantiate" [ "-E", e ]
      >>= toNixFilePath StorePath

-- | Just tests if the expression can be evaluated.
-- That doesn’t mean it has to instantiate however.
eval :: MonadIO m => NixExpr -> NixAction InstantiateError m ()
eval (NixExpr e) = do
  _instantiateOutput <- mapActionError parseInstantiateError
       $ evalNixOutput "nix-instantiate" [ "--eval", "-E", e ]
  pure ()

parseInstantiateError :: Text -> InstantiateError
parseInstantiateError
  (stripPrefix "error: expression does not evaluate to a derivation"
               -> Just _) = NotADerivation
parseInstantiateError _   = UnknownInstantiateError


------------------------------------------------------------------------------
-- Realizing

data RealizeError = UnknownRealizeError deriving (Show, Eq)

-- | Finally derivations are realized into full store outputs.
-- This will typically take a while so it should be executed asynchronously.
realize :: MonadIO m => StorePath Derivation -> NixAction RealizeError m (StorePath Realized)
realize (StorePath d) =
     storeOp [ "-r", Text.pack d ]

-- | Copy the given file or folder to the nix store and return it’s path.
addToStore :: MonadIO m => FilePath -> NixAction RealizeError m (StorePath Realized)
addToStore fp = storeOp [ "--add", Text.pack fp ]

storeOp :: (MonadIO m) => [Text] -> NixAction RealizeError m (StorePath Realized)
storeOp op =
  mapActionError (const UnknownRealizeError)
    $ evalNixOutput "nix-store" op
      >>= toNixFilePath StorePath

------------------------------------------------------------------------------
-- Convenience

-- | Combines all error types that could happen.
data NixError
  = ParseError ParseError
  | InstantiateError InstantiateError
  | RealizeError RealizeError deriving (Show, Eq)

-- | A convenience function to directly realize a nix expression.
-- Any errors are put into a combined error type.
parseInstRealize :: (MonadIO m) => Text -> NixAction NixError m (StorePath Realized)
parseInstRealize = mapActionError ParseError . parseNixExpr
               >=> mapActionError InstantiateError . instantiate
               >=> mapActionError RealizeError . realize

------------------------------------------------------------------------------
-- Helpers

-- | Take args and return either error message or output path
evalNixOutput :: (MonadIO m)
              => Text
              -- ^ name of executable
              -> [Text]
              -- ^ arguments
              -> NixAction Text m Text
              -- ^ error: (stderr, errormsg), success: path
evalNixOutput = Helpers.readProcess (\(out, err) -> \case
  ExitFailure _ -> throwE $
    case
      err
        & Text.lines
        & dropWhile (not . isPrefixOf "error: ")
        & List.intersperse "\n"
        & mconcat of
      "" -> "nix didn’t output any error message"
      s  -> s
  ExitSuccess -> tryLast
      "nix didn’t output a store path" (Data.Text.lines out))


-- | Apply filePath p to constructor a if it’s a valid filepath
toNixFilePath :: Monad m => (String -> a) -> Text -> NixAction Text m a
toNixFilePath a p = NixAction $
  if FilePath.isValid (Text.unpack p) then pure $ a (Text.unpack p)
  else throwError $ NixActionError
          { actionStderr = nostderr
          , actionError = p <> " is not a filepath!" }
  where nostderr = mempty
