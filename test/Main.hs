module Main where

import Test.Framework (defaultMain)

import qualified Bippy.Tests (tests)

main :: IO ()
main = defaultMain Bippy.Tests.tests
