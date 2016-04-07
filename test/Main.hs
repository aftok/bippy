module Main where

import Test.Framework (defaultMain)

import qualified Network.Bippy.Tests (tests)

main :: IO ()
main = defaultMain Network.Bippy.Tests.tests
