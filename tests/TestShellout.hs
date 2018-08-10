{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
module TestShellout where

import Protolude
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
  , testGroup "realizing"
    [ nixpkgsExists
    , multilineErrors
    , helloWorld
    , copyTempfileToStore ]
  ]

syntaxError, infiniteRecursion, notADerivation, someDerivation :: TestTree
nixpkgsExists, multilineErrors, helloWorld, copyTempfileToStore :: TestTree

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

multilineErrors = testCase "nixpkgs multiline stderr it parsed"
  $ parseEval "builtins.abort ''wow\nsuch error''"
  `isE` (Left $ OtherInstantiateError
           "error: evaluation aborted with the following error \
           \message: 'wow\nsuch error'")

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
parseInstLike like =
  first (\_ -> OtherInstantiateError "parse failed :(.")
              . parseNixExpr >=> like

isE :: (Eq e, Eq a, Show a, Show e) => NixAction e a -> Either e a -> Assertion
isE na eith = runExceptT (unNixAction na) >>= (@?= eith) . first snd

assertNoFailure :: Show e => NixAction e a -> Assertion
assertNoFailure na = do
  ei <- runExceptT (unNixAction na)
  case ei of
    (Left (_, e)) -> assertFailure $ show e
    (Right _) -> pure ()
