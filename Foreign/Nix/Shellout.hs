{-# LANGUAGE NoImplicitPrelude, ViewPatterns, TupleSections, OverloadedStrings #-}
module Foreign.Nix.Shellout
  ( NixExpr, parseNixExpr
  , instantiate
  , StorePath, Derivation, Build
  , NixError(..), ParseError(..)
  ) where

import Protolude
-- import Data.List (stripPrefix)
-- import Data.String (lines, String)
import Data.Text (stripPrefix, lines)
import System.Process (readProcessWithExitCode)


-------------------------------------------------------------------------------
newtype NixExpr = NixExpr Text deriving (Show, Eq)
data ParseError = SyntaxError Text
                | OtherParseError Text deriving (Show, Eq)

parseNixExpr :: Text -> IO (Either ParseError NixExpr)
parseNixExpr e = do
  bimap parseParseError NixExpr
    <$> evalInstantiateOutput [ "--parse", "-E", e ]


-------------------------------------------------------------------------------
type StorePath a = FilePath
data Derivation
data Build

data NixError = NotADerivation
              | UnexpectedError Text
              | OtherInstantiateError Text deriving (Show, Eq)

instantiate :: NixExpr -> IO (Either NixError (StorePath Derivation))
instantiate e = undefined


-------------------------------------------------------------------------------
parseInstantiateError :: Text -> NixError
parseInstantiateError
  (stripPrefix "error: expression does not evaluate to a derivation"
               -> Just _) = NotADerivation
parseInstantiateError s   = OtherInstantiateError $ s

parseParseError :: Text -> ParseError
parseParseError
  (stripPrefix "error: syntax error, "
               -> Just mes) = SyntaxError $ mes
parseParseError s           = OtherParseError $ s


-------------------------------------------------------------------------------
-- | Take args and return either error message or output path
evalInstantiateOutput :: [Text] -> IO (Either Text Text)
evalInstantiateOutput args = do
  (exc, out, err) <- readProcessWithExitCode "nix-instantiate" (map toS args) ""
  pure $ case exc of
    ExitFailure _ -> Left $ lastDef
                       "nix didn’t output any error message" (lines $ toS err)
    ExitSuccess   -> Left $ lastDef
                       "nix didn’t output a derivation path" (lines $ toS out)
