{-|
Module      : Foreign.Nix.Shellout
Description : Interface to the nix package manager that calls the CLI
Copyright   : Profpatsch, 2016
License     : GPL-3
Stability   : experimental
Portability : nix 1.11.x (maybe 1.x.x)

This module directly calls the nix command line to convert
textual nix expressions to derivations & realized storepaths.
-}
{-# LANGUAGE NoImplicitPrelude, ViewPatterns, OverloadedStrings, TupleSections #-}
module Foreign.Nix.Shellout
  ( NixAction
  , NixExpr, parseNixExpr
  , instantiate, realize, eval, addToStore
  , parseInstRealize, liftError
  , StorePath(fromStorePath), Derivation, Realized
  , InstantiateError(..), ParseError(..), RealizeError(..), NixError(..)
  ) where

import Protolude hiding (show, isPrefixOf)
import Control.Error hiding (bool, err)
import Data.String (String)
import Data.Text (stripPrefix, lines, isPrefixOf)
import System.FilePath (isValid)
import System.Process (readProcessWithExitCode)
import Text.Show (Show(..))

------------------------------------------------------------------------------
-- Parsing

-- TODO: is it good to put stderr into the error?
-- TODO: newtype?
-- | An ExceptT that also provides the whole nix stderr (for debugging purposes)
type NixAction e a = ExceptT (Text, e) IO a

newtype NixExpr = NixExpr Text deriving (Show, Eq)

data ParseError = SyntaxError Text
                | OtherParseError Text deriving (Show, Eq)

-- | Parse a nix expression and check for syntactic validity.
parseNixExpr :: Text -> NixAction ParseError NixExpr
parseNixExpr e =
  bimapExceptT (fmap parseParseError) NixExpr
    $ evalNixOutput "nix-instantiate" [ "--parse", "-E", e ]


parseParseError :: Text -> ParseError
parseParseError
  (stripPrefix "error: syntax error, "
               -> Just mes) = SyntaxError $ mes
parseParseError s           = OtherParseError $ s


------------------------------------------------------------------------------
-- Instantiating

newtype StorePath a = StorePath { fromStorePath :: FilePath }
                        deriving (Eq, Show)
data Derivation
data Realized

data InstantiateError = NotADerivation
              | UnexpectedError Text
              | OtherInstantiateError Text deriving (Show, Eq)

-- | Instantiate a parsed expression into a derivation.
instantiate :: NixExpr -> NixAction InstantiateError (StorePath Derivation)
instantiate (NixExpr e) =
  fmapLT (fmap parseInstantiateError)
    $ evalNixOutput "nix-instantiate" [ "-E", e ]
      >>= toNixFilePath StorePath

-- | Just tests if the expression can be evaluated.
-- That doesn’t mean it has to instantiate however.
eval :: NixExpr -> NixAction InstantiateError ()
eval (NixExpr e) = do
  _ <- fmapLT (fmap parseInstantiateError)
    $ evalNixOutput "nix-instantiate" [ "--eval", "-E", e ]
  pure ()


parseInstantiateError :: Text -> InstantiateError
parseInstantiateError
  (stripPrefix "error: expression does not evaluate to a derivation"
               -> Just _) = NotADerivation
parseInstantiateError s   = OtherInstantiateError $ s


------------------------------------------------------------------------------
-- Realizing

data RealizeError = OtherRealizeError Text deriving (Show, Eq)

-- | Finally derivations are realized into full store outputs.
-- This will typically take a while so it should be executed asynchronously.
realize :: StorePath Derivation -> NixAction RealizeError (StorePath Realized)
realize (StorePath d) =
     storeOp [ "-r", toS d ]

addToStore :: FilePath -> NixAction RealizeError (StorePath Realized)
addToStore fp = storeOp [ "--add", toS fp ]

storeOp :: [Text] -> NixAction RealizeError (StorePath Realized)
storeOp op =
  fmapLT (fmap OtherRealizeError)
    $ evalNixOutput "nix-store" op
      >>= toNixFilePath StorePath

------------------------------------------------------------------------------
-- Convenience

data NixError = ParseError ParseError
              | InstantiateError InstantiateError
              | RealizeError RealizeError deriving (Show, Eq)

-- | A convenience function to directly realize a nix expression.
-- Any errors are put into a combined error type.
parseInstRealize :: Text -> NixAction NixError (StorePath Realized)
parseInstRealize = liftError ParseError . parseNixExpr
               >=> liftError InstantiateError . instantiate
               >=> liftError RealizeError . realize

liftError :: (e -> NixError) -> NixAction e a -> NixAction NixError a
liftError f a = withExceptT (fmap f) a

------------------------------------------------------------------------------
-- Helpers

-- | Take args and return either error message or output path
evalNixOutput :: Text
              -- ^ name of executable
              -> [Text]
              -- ^ arguments
              -> NixAction Text Text
              -- ^ error: (stderr, errormsg), success: path
evalNixOutput exec args = do
  (exc, out, err) <- liftIO
    $ readProcessWithExitCode (toS exec) (map toS args) ""
  withExceptT (toS err,) $ case exc of
    ExitFailure _ -> throwE $
                       case mconcat . intersperse "\n"
                          . dropWhile (not.isPrefixOf "error: ")
                          . lines $ toS err of
                         "" -> "nix didn’t output any error message"
                         s  -> s
    ExitSuccess   -> tryLast
                       "nix didn’t output a store path" (lines $ toS out)

-- | Apply filePath p to constructor a if it’s a valid filepath
toNixFilePath :: (String -> a) -> Text -> NixAction Text a
toNixFilePath a p@(toS -> ps) =
  if isValid ps then (pure $ a ps)
  else (throwE $ (nostderr, p <> " is not a filepath!"))
  where nostderr = mempty
