{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module TestShellout where

import qualified Data.Text as T
import System.IO (openTempFile, hClose)
import System.Directory (getTemporaryDirectory)

import Test.Tasty
import Test.Tasty.HUnit

import Foreign.Nix.Shellout
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>=>))
import Control.Exception (SomeException, try, Exception (displayException))
import Data.Function ((&))
import qualified Data.List as List


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
  , testGroup "executable paths"
    [ badInstantiateFilePath ]
  ]


syntaxError :: TestTree
syntaxError = testCase "syntax error"
  $ parseNixExpr ";" `isENoFail`
      Left ("", SyntaxError "unexpected ';', at (string):1:1")

infiniteRecursion :: TestTree
infiniteRecursion = testCase "infinite recursion"
  $ parseInst "let a = a; in a"
  `isE` Left ( "error: infinite recursion encountered, at (string):1:10"
              , UnknownInstantiateError)

notADerivation :: TestTree
notADerivation = testCase "not a derivation"
  $ parseInst "42"
  `isE` Left ("", NotADerivation)

someDerivation :: TestTree
someDerivation = testCase "a basic derivation"
  $ assertNoFailure $ parseInst
      "derivation { name = \"foo\"; builder = \" \"; system = \" \"; }"

nixpkgsExists :: TestTree
nixpkgsExists = testCase "nixpkgs is accessible"
  $ assertNoFailure $ parseEval "import <nixpkgs> {}"

multilineErrors :: TestTree
multilineErrors = testCase "nixpkgs multiline stderr it parsed"
  $ parseEval "builtins.abort ''wow\nsuch error''"
  `isE` Left
           ( "error: evaluation aborted with the following error \
             \message: 'wow\nsuch error'"
           , UnknownInstantiateError)

helloWorld :: TestTree
helloWorld = testCase "build the GNU hello package"
  $ assertNoFailure $ parseInstRealize "with import <nixpkgs> {}; hello"

copyTempfileToStore :: TestTree
copyTempfileToStore = testCase "copy a temporary file to store"
  $ assertNoFailure $ do
    (fp, h) <- liftIO $
      getTemporaryDirectory >>= flip openTempFile "store-test"
    liftIO $ hClose h
    addToStore fp

badInstantiateFilePath :: TestTree
badInstantiateFilePath = testCase "not a derivation"
  $ isExcBad "No such file or directory" (parseInst "42")
  where
    isExcBad = isException (defaultRunOptions {
      executables = defaultExecutables {
        exeNixInstantiate = Just "/path/does/not/exist"
      }
    })

--------------------------------------------------------------------
-- Helpers

parseInst :: Text -> NixAction InstantiateError IO (StorePath Derivation)
parseInst = parseInstLike instantiate

parseEval :: Text -> NixAction InstantiateError IO ()
parseEval = parseInstLike eval

parseInstLike :: (NixExpr -> NixAction InstantiateError IO a)
              -> Text
              -> NixAction InstantiateError IO a
parseInstLike like =
  mapActionError (\_ -> UnknownInstantiateError)
              . parseNixExpr >=> like

isE :: (Eq a, Eq e, Show a, Show e)
    => NixAction e IO a
    -> Either (Text, e) a
       -- ^ Left (subset of stdout, error)
    -> Assertion
isE = isE' defaultRunOptions

isE' :: (Eq a, Eq e, Show a, Show e)
    => RunOptions IO
    -> NixAction e IO a
    -> Either (Text, e) a
       -- ^ Left (subset of stdout, error)
    -> Assertion
isE' runOptions na match = runNixAction runOptions na >>= \res ->
  case (match, res) of
    (Right a, Right b) -> a @=? b
    (match', res') -> check match' res'

isException :: RunOptions IO -> [Char] -> NixAction e IO a -> IO ()
isException runOptions matchText na = try @SomeException (runNixAction runOptions na) >>= \case
  Left exc -> do
    let text = exc & displayException
    assertBool ("threw exception, but did not match the given substring\nshould have contained: " <> matchText <> "\nbut was: " <> text)
      $ matchText `List.isInfixOf` text
  Right _a -> assertFailure "runNixAction should have thrown an exception"

isENoFail :: (Eq e, Show a, Show e)
          => NixAction e IO a
          -> Either (Text, e) a
            -- ^ Left (subset of stdout, error)
          -> Assertion
isENoFail na match = runNixAction defaultRunOptions na >>= check match

check :: (Eq a, Show a)
      => Either (Text, a) _x
      -> Either (NixActionError a) _x
      -> Assertion
check (Left (outMatch, err))
      (Left NixActionError
                { actionStderr = out
                , actionError = err' }) = do
  assertBool "stderr not matched" (outMatch `T.isInfixOf` out)
  err @=? err'
check (Right _) (Left _) =
  assertFailure "output should have succeeded, but it failed"
check (Left _) (Right _) =
  assertFailure "output should have failed, but it succeeded"
check _ _ = error "handled by isE"

assertNoFailure :: Show e => NixAction e IO a -> Assertion
assertNoFailure na = do
  ei <- runNixAction defaultRunOptions na
  case ei of
    (Left naErr) -> assertFailure $ show $ actionError naErr
    (Right _) -> pure ()
