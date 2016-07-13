{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
module TestShellout where

import Protolude
import Control.Error hiding (isLeft)

import Test.Tasty
import Test.Tasty.HUnit

import Foreign.Nix.Shellout

shelloutTests = testGroup "shellout tests"
  [ testGroup "parsing"
    [ syntaxError ]
  , testGroup "evaluation"
    [ infiniteRecursion
    , notADerivation
    , someDerivation ]
  ]

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

someDerivation = testCase "a basic derivation" $ do
  e <- runExceptT $ (parseInst
    "derivation { name = \"foo\"; builder = \" \"; system = \" \"; }")
  case e of
    (Left e)  -> assertFailure $ show e
    (Right _) -> pure ()



parseInst :: Text -> ExceptT NixError IO (StorePath Derivation)
parseInst e = (catchParse $ parseNixExpr e) >>= instantiate

catchParse :: ExceptT ParseError IO a -> ExceptT NixError IO a
catchParse exT = catchE exT
  (\e -> throwE $ OtherInstantiateError "error in parse!")

isE :: (Eq e, Eq a, Show a, Show e) => ExceptT e IO a -> Either e a -> Assertion
isE exT eith = runExceptT exT >>= (@?= eith)
