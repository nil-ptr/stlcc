module Main where

import           STLCCTest.Generators.Vec
import           STLCCTest.Test.Parse     (parserSpec)
import           STLCCTest.Test.Util.Nat  (natSpec)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  natSpec
  parserSpec
