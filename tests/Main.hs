module Main where

import TestShellout
import Test.Tasty

main :: IO ()
main = defaultMain shelloutTests
