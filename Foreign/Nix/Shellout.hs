{-# LANGUAGE NoImplicitPrelude, ViewPatterns, TupleSections, OverloadedStrings #-}
module Foreign.Nix.Shellout
  ( NixExpr, parseNixExpr
  , instantiate
  , StorePath, Derivation, Build
  , NixError(..), ParseError(..)
  ) where

import Protolude
import Data.Text (stripPrefix, lines)
import System.Process (readProcessWithExitCode)
import System.FilePath (isValid)
import Control.Error hiding (bool)


-------------------------------------------------------------------------------
newtype NixExpr = NixExpr Text deriving (Show, Eq)
data ParseError = SyntaxError Text
                | OtherParseError Text deriving (Show, Eq)

parseNixExpr :: Text -> ExceptT ParseError IO NixExpr
parseNixExpr e = do
  bimapExceptT parseParseError NixExpr
    $ evalInstantiateOutput [ "--parse", "-E", e ]


-------------------------------------------------------------------------------
newtype StorePath a = StorePath FilePath deriving (Eq, Show)
data Derivation
data Build

data NixError = NotADerivation
              | UnexpectedError Text
              | OtherInstantiateError Text deriving (Show, Eq)

instantiate :: NixExpr -> ExceptT NixError IO (StorePath Derivation)
instantiate (NixExpr e) =
  fmapLT parseInstantiateError
    $ evalInstantiateOutput [ "-E", e ]
    >>= (\t@(toS -> out)
          -> if isValid out
             then (pure $ StorePath out)
             else (throwE $ t <> " is not a filepath!"))


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
evalInstantiateOutput :: [Text] -> ExceptT Text IO Text
evalInstantiateOutput args = do
  (exc, out, err) <- liftIO
    $ readProcessWithExitCode "nix-instantiate" (map toS args) ""
  case exc of
    ExitFailure _ -> tryLast
                       "nix didn’t output any error message" (lines $ toS err)
                       >>= throwE
    ExitSuccess   -> tryLast
                       "nix didn’t output a derivation path" (lines $ toS out)
