module Main where

import           STLCCTest.Test.Util.Nat (natSpec)

import           Test.Hspec

main :: IO ()
main = do
  putStrLn "here"
  hspec $ natSpec
