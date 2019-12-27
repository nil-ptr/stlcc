-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Generators.ASTGen
-- Copyright   :  Nils Gustafsson 2019
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple attempt at generating arbitrary AST-like structures using
-- Prüfer-sequence-like methods.
module STLCCTest.Generators.ASTGen
  (
  -- * Exported Items
    generateIndicesSized
  , mkParentListSized
  , genASTShapeSequence
) where

import  Test.QuickCheck.Gen


----------------------------------------------------------------------
---                       Generating Numbers                       ---
----------------------------------------------------------------------

-- | Generates the indices with which to access the parent list.
generateIndicesSized :: Int -- ^ Size parameter
                     -> Int -- ^ Maximum arity
                     -> Gen [Int]
generateIndicesSized s m = inner 1
  where inner n | n < s = do
                    x <- choose (0,(m-1)*n)
                    xs <- inner (n + 1)
                    pure (x : xs)
                | otherwise = pure []


-- | Construct the parent list needed.
mkParentListSized :: Int -- ^ Size parameter
                  -> Int -- ^ Maximum arity
                  -> [Int]
mkParentListSized s p
  | p <= 0 = error "bad max arity in mkParentList"
  | otherwise = go s p []
  where go gs gp | gp > 0 && gs >= 0 = go gs (gp - 1) . (gs :)
                 | gs > 0 = go (gs - 1) p
                 | otherwise = id




genASTShapeSequence :: Int -- ^ Maximum arity
                    -> Gen [Int]
genASTShapeSequence a = sized inner
  where inner n = do
          let pl = mkParentListSized (n - 1) a
          ix <- generateIndicesSized n a
          pure (resolve ix pl)

        remove i l = let (h,(t:ts)) = splitAt i l
                     in (t, h ++ ts)
        resolve (x:xs) l = let (v,l') = remove x l
                           in v : resolve xs l'
        resolve [] _ = []
