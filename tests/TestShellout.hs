{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
module TestShellout where

import Protolude
import Control.Error hiding (isLeft)
import System.IO (openTempFile, hClose)
import System.Directory (getTemporaryDirectory)

import Test.Tasty
import Test.Tasty.HUnit

import Foreign.Nix.Shellout


shelloutTests :: TestTree
shelloutTests = testGroup "shellout tests"
  [ testGroup "parsing"
    [ syntaxError ]
  , testGroup "evaluating"
    [ infiniteRecursion
    , notADerivation
    , someDerivation ]
  , testGroup "realising"
    [ nixpkgsExists
    , helloWorld
    , copyTempfileToStore ]
  ]

syntaxError, infiniteRecursion, notADerivation, someDerivation :: TestTree
nixpkgsExists, helloWorld, copyTempfileToStore :: TestTree

syntaxError = testCase "syntax error"
  $ parseNixExpr ";"
  `isE` (Left $ SyntaxError "unexpected ';', at (string):1:1")

infiniteRecursion = testCase "infinite recursion"
  $ parseInst "let a = a; in a"
  `isE` (Left $ OtherInstantiateError
          "error: infinite recursion encountered, at (string):1:10")

notADerivation = testCase "not a derivation"
  $ parseInst "42"
  `isE` (Left $ NotADerivation)

someDerivation = testCase "a basic derivation"
  $ assertNoFailure $ parseInst
      "derivation { name = \"foo\"; builder = \" \"; system = \" \"; }"

nixpkgsExists = testCase "nixpkgs is accessible"
  $ assertNoFailure $ parseEval "import <nixpkgs> {}"

helloWorld = testCase "build the GNU hello package"
  $ assertNoFailure $ parseInstRealize "with import <nixpkgs> {}; hello"

copyTempfileToStore = testCase "copy a temporary file to store"
  $ assertNoFailure $ do
    (fp, h) <- liftIO $
      getTemporaryDirectory >>= flip openTempFile "store-test"
    liftIO $ hClose h
    addToStore fp


--------------------------------------------------------------------
-- Helpers

parseInst :: Text -> NixAction InstantiateError (StorePath Derivation)
parseInst = parseInstLike instantiate

parseEval :: Text -> NixAction InstantiateError ()
parseEval = parseInstLike eval

parseInstLike :: (NixExpr -> NixAction InstantiateError a)
              -> Text
              -> NixAction InstantiateError a
parseInstLike like = withExceptT (const $ OtherInstantiateError "parse failed :(.")
              . parseNixExpr >=> like

isE :: (Eq e, Eq a, Show a, Show e) => NixAction e a -> Either e a -> Assertion
isE exT eith = runExceptT exT >>= (@?= eith)

assertNoFailure :: Show e => NixAction e a -> Assertion
assertNoFailure exT = do
  ei <- runExceptT exT
  case ei of
    (Left e) -> assertFailure $ show e
    (Right _) -> pure ()
