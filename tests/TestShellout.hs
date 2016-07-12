{-# LANGUAGE OverloadedStrings #-}
module TestShellout where

import Test.Tasty
import Test.Tasty.HUnit

import Foreign.Nix.Shellout

shelloutTests = testGroup "shellout tests"
  [ syntaxError ]

syntaxError = testCase "syntax error" $ do
  res <- parseNixExpr ";"
  res @?= Left (SyntaxError "unexpected ';', at (string):1:1")
