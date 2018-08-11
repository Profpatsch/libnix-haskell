module Main where

import Prelude
import TestShellout
import Test.Tasty

main :: IO ()
main = defaultMain shelloutTests
