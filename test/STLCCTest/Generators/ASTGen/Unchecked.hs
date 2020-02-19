{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeApplications     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Generators.ASTGen.Unchecked
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators for the AST type defined in "STLCC.AST.Unchecked".
module STLCCTest.Generators.ASTGen.Unchecked
  (
  -- * Exported Items
    genLamAndConst
  , deShadow


) where

import           Test.QuickCheck

import           STLCCTest.Generators.ASTGen
import           STLCCTest.Generators.Nat()

import           STLCC.AST.Unchecked
import STLCC.Util.Vec
import           STLCC.Util.Nat

import           Data.Text
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as Vector


----------------------------------------------------------------------
---                 Errors Specific To This Module                 ---
----------------------------------------------------------------------

badArityErr :: String -> a
badArityErr s = error $ "Bad arity in " ++ s


----------------------------------------------------------------------
---                        Sequence Rejiging                       ---
----------------------------------------------------------------------

flavGenerator :: (Int,Int,Int)
              -> Int
              -> Gen Int
flavGenerator (i,_,_) 0 = choose (0,i-1)
flavGenerator (_,i,_) 1 = choose (0,i-1)
flavGenerator (_,_,i) 2 = choose (0,i-1)
flavGenerator _ _       = badArityErr "flavGenerator"

rejigSeq :: Vector Int -> (Int -> Gen Int) -> Gen (Vector (Int,Int,Int,Int))
rejigSeq v g = Vector.generateM (Vector.length v + 1) go
  where go i =
          let ixs = Vector.findIndices (==i) v
              ixslen = Vector.length ixs
              tryIx ii | ii < ixslen = (ixs Vector.! ii) +1
                       | otherwise = -100
          in do
            flav <- g ixslen
            pure (ixslen, flav, tryIx 0, tryIx 1)

mkASTBuildSeq :: (Int,Int,Int)
              -> Vector Int
              -> Gen (Vector (Int,Int,Int,Int))
mkASTBuildSeq fs vs =
  rejigSeq vs (flavGenerator fs)


----------------------------------------------------------------------
---                 Overloaded Arity-based Builders                ---
----------------------------------------------------------------------


data UExpGenConfig = 'LamConstApp
data GenConf (c :: UExpGenConfig) = GC !Int

class UExpGen (c :: UExpGenConfig) (n :: Nat) where
  build0 :: GenConf c -> Gen (UExp n)
  build1 :: GenConf c -> Gen (Either (UExp n -> UExp n) (UExp ('S n) -> UExp n))
  build2 :: GenConf c -> Gen (UExp n -> UExp n -> UExp n)

-- | Adjust all de Brujin indices to account for shadowing. This might
-- be needed since the arbitrary instance for 'Fin' could give us
-- indices that refer to variables that are unreachable due to
-- shadowing.
--
-- This operation is @O(n*n)@ in the worst case: all nodes in the ast
-- denote lamda abstractions. This is unlikely in practice. A more
-- fair estimate of the average complexity is probably somewhere in
-- the @O(n * log n)@ area. I haven't actually tried to compute this
-- asymptote though, so who knows.
deShadow :: UExp 'Z -> UExp 'Z
deShadow = inner VNil
  where inner :: Vec Text n -> UExp n -> UExp n
        -- VNil case: Absurd.
        inner VNil (UVar n) = absurdFinZero n

        -- Cons case.
        inner vs@(_ ::: _) (UVar n) =
          let (_,ix) = lookupShadowed n vs
          in UVar ix

        -- Lambdas
        inner vs (ULam s ty e) = ULam s ty (inner (s:::vs) e)

        -- Non-trivial boring cases
        inner vs (UApp a b)       = UApp (inner vs a) (inner vs b)
        inner vs (UAnnot term ty) = UAnnot (inner vs term) ty


        -- Trivial cases
        inner _ x@(UGlobal _) = x
        inner _ x@(UNatE _)   = x
        inner _ x@(UIntE _)   = x
        inner _ x@(UBoolE _)  = x


        lookupShadowed :: Fin ('S n) -> Vec Text ('S n) -> (Text, Fin ('S n))
        lookupShadowed FZ (v ::: _) = (v, FZ)
        lookupShadowed (FS FZ) (v ::: vs) =
          let (t, fn') = lookupShadowed FZ vs
          in if t == v
             then (t, FZ)
             else (t, FS fn')
        lookupShadowed (FS fn@(FS _)) (v ::: vs) =
          let (t, fn') = lookupShadowed fn vs
          in if t == v
             then (t, FZ)
             else (t, FS fn')


----------------------------------------------------------------------
---               Lambdas, Constants and Application               ---
----------------------------------------------------------------------

instance UExpGen 'LamConstApp 'Z where
  build0 (GC 0) = UBoolE <$> arbitrary
  build0 (GC 1) = UBoolE <$> arbitrary

  build1 (GC 0) = do
    s <- elements ['a'..'z']
    pure (Right (ULam (pack [s]) Nothing))

  build2 (GC 0) = pure UApp

instance KnownSNat n => UExpGen 'LamConstApp ('S n) where
  build0 (GC 0) = UBoolE <$> arbitrary
  build0 (GC 1) = UVar <$> arbitrary

  build1 (GC 0) = do
    s <- elements ['a'..'z']
    pure (Right (ULam (pack [s]) Nothing))

  build2 (GC 0) = pure UApp

genLamAndConst :: Gen (UExp 'Z)
genLamAndConst = deShadow <$> do
  sv <- genASTShapeSequence' 2
  con <- mkASTBuildSeq (2,1,1) sv

  bt con 0

  where bt :: UExpGen 'LamConstApp x
           => KnownSNat x
           => Vector (Int,Int,Int,Int) -> Int -> Gen (UExp x)
        bt cs i =
          let (a,f,c0,c1) = cs Vector.! i
          in case a of
               0 -> build0 (GC @'LamConstApp f)

               1 -> do
                 x <- build1 (GC @'LamConstApp f)
                 case x of
                   Left l -> l <$> (bt cs c0)
                   Right r -> r <$> (bt cs c0)
               2 -> build2 (GC @'LamConstApp f) <*>
                    bt cs c0 <*>
                    bt cs c1
               _ -> badArityErr "genLamAndConst.bt"
