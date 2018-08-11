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
, StorePath(fromStorePath), Derivation, Realized
, NixExpr
, runNixAction, NixAction(..)
) where

import Protolude hiding (show, isPrefixOf)
import Control.Error hiding (bool, err)
import Data.String (String)
import Data.Text (stripPrefix, lines, isPrefixOf)
import System.FilePath (isValid)
import Text.Show (Show(..))

import qualified Foreign.Nix.Shellout.Helpers as Helpers
import Foreign.Nix.Shellout.Types

------------------------------------------------------------------------------
-- Parsing

-- | A sucessfully parsed nix expression.
newtype NixExpr = NixExpr Text deriving (Show, Eq)

data ParseError
  = SyntaxError Text
    -- ^ the input string was not a syntactically valid nix expression
  | UnknownParseError
    -- ^ catch-all error
  deriving (Show, Eq)

-- | Parse a nix expression and check for syntactic validity.
parseNixExpr :: Text -> NixAction ParseError NixExpr
parseNixExpr e =
  bimap parseParseError NixExpr
    $ evalNixOutput "nix-instantiate" [ "--parse", "-E", e ]


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
instantiate :: NixExpr -> NixAction InstantiateError (StorePath Derivation)
instantiate (NixExpr e) =
  first parseInstantiateError
    $ evalNixOutput "nix-instantiate" [ "-E", e ]
      >>= toNixFilePath StorePath

-- | Just tests if the expression can be evaluated.
-- That doesn’t mean it has to instantiate however.
eval :: NixExpr -> NixAction InstantiateError ()
eval (NixExpr e) =
  void $ first parseInstantiateError
       $ evalNixOutput "nix-instantiate" [ "--eval", "-E", e ]

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
realize :: StorePath Derivation -> NixAction RealizeError (StorePath Realized)
realize (StorePath d) =
     storeOp [ "-r", toS d ]

-- | Copy the given file or folder to the nix store and return it’s path.
addToStore :: FilePath -> NixAction RealizeError (StorePath Realized)
addToStore fp = storeOp [ "--add", toS fp ]

storeOp :: [Text] -> NixAction RealizeError (StorePath Realized)
storeOp op =
  first (const UnknownRealizeError)
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
parseInstRealize :: Text -> NixAction NixError (StorePath Realized)
parseInstRealize = first ParseError . parseNixExpr
               >=> first InstantiateError . instantiate
               >=> first RealizeError . realize

------------------------------------------------------------------------------
-- Helpers

-- | Take args and return either error message or output path
evalNixOutput :: Text
              -- ^ name of executable
              -> [Text]
              -- ^ arguments
              -> NixAction Text Text
              -- ^ error: (stderr, errormsg), success: path
evalNixOutput = Helpers.readProcess (\(out, err) -> \case
  ExitFailure _ -> throwE $
    case mconcat . intersperse "\n"
      . dropWhile (not . isPrefixOf "error: ")
      . lines $ toS err of
      "" -> "nix didn’t output any error message"
      s  -> s
  ExitSuccess -> tryLast
      "nix didn’t output a store path" (lines $ toS out))


-- | Apply filePath p to constructor a if it’s a valid filepath
toNixFilePath :: (String -> a) -> Text -> NixAction Text a
toNixFilePath a p@(toS -> ps) = NixAction $
  if isValid ps then (pure $ a ps)
  else (throwE $ (nostderr, p <> " is not a filepath!"))
  where nostderr = mempty
