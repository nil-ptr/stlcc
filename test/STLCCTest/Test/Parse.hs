-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Tests.Parse
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for "STLCC.Parse".
module STLCCTest.Test.Parse
  (
    parserSpec
  ) where


import           STLCCTest.Generators.ASTGen.Unchecked
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.QuickCheck

import           Data.Text                             as Text

import           STLCC.AST.Unchecked
import           STLCC.Parse

parserSpec :: Spec
parserSpec = context "in STLCC.Parse" closedExprSpec




----------------------------------------------------------------------
---                       Closed Expressions                       ---
----------------------------------------------------------------------

closedExprSpec :: Spec
closedExprSpec = describe "STLCC.Parse.closedExpression" $ do
  it "correctly roundtrips expressions containing only lambdas and vars" $
    forAll genLamAndConst $ \expr ->
    label ("\n\n" ++ show expr ++ "\n\n" ++prettyNamed expr ++ "\n" ++ pretty expr) $
    parse closedExpression "(test)" (Text.pack $ prettyNamed expr) `shouldParse` expr
