{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Generators.Vec
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators for the 'STLCC.Util.Vec.Vec' type defined in
-- "STLCC.Util.Vec".
--
-- Orphan instances ahead.
module STLCCTest.Generators.Vec
  (
  -- * Exported Items

) where

import           STLCC.Util.Nat
import           STLCC.Util.Vec           as Vec
import           STLCCTest.Generators.Nat
import           Test.QuickCheck

----------------------------------------------------------------------
---                          Known Length                          ---
----------------------------------------------------------------------



buildArbitraryVec :: SNat n -> Gen a -> Gen (Vec a n)
buildArbitraryVec SZero _ = pure VNil
buildArbitraryVec (SSucc s) gg = do
  v <- gg
  vs <- buildArbitraryVec s gg
  pure (v ::: vs)

instance (KnownSNat n, Arbitrary a) => Arbitrary (Vec a n) where
  arbitrary = buildArbitraryVec snat arbitrary
  shrink x = transposeWithList (Vec.map shrink x)


----------------------------------------------------------------------
---                        Arbitrary Length                        ---
----------------------------------------------------------------------


buildForSomeNat :: Fin ('S n) -> Gen a -> Gen (ForSomeNat (Vec a))
buildForSomeNat FZ _ = pure (MkForSomeNat VNil)
buildForSomeNat (FS FZ) gg = gg >>= \v -> pure (MkForSomeNat (v ::: VNil))
buildForSomeNat (FS fn@(FS _)) gg = do
  v <- gg
  MkForSomeNat vs <- buildForSomeNat fn gg
  pure (MkForSomeNat (v ::: vs))

buildForSomeBound :: Fin ('S n) -> Gen a -> Gen (ForSomeNatLTE n (Vec a))
buildForSomeBound FZ _ = pure (MkForSomeNatLTE LTEZero VNil)
buildForSomeBound (FS FZ) gg = do
  v <- gg
  pure (MkForSomeNatLTE (LTESucc LTEZero) (v ::: VNil))
buildForSomeBound (FS fn@(FS _)) gg = do
  v <- gg
  MkForSomeNatLTE prf vs <- buildForSomeBound fn gg
  pure (MkForSomeNatLTE (LTESucc prf) (v ::: vs))

shrinkForSomeBound :: (a -> [a])
                   -> ForSomeNatLTE n (Vec a)
                   -> [ForSomeNatLTE n (Vec a)]
shrinkForSomeBound _ (MkForSomeNatLTE _ VNil) = []
shrinkForSomeBound f (MkForSomeNatLTE (LTESucc prf) (v ::: vs)) =
  MkForSomeNatLTE LTEZero VNil : rest
  where rest = do
          v' <- f v
          MkForSomeNatLTE prf' vs' <- shrinkForSomeBound
                                          f
                                         (MkForSomeNatLTE prf vs)
          pure (MkForSomeNatLTE (LTESucc prf') (v' ::: vs'))



instance (Arbitrary a, KnownSNat n) => Arbitrary (ForSomeNatLTE n (Vec a)) where
  arbitrary = do
    fn <- arbitrary
    buildForSomeBound fn arbitrary
  shrink x = shrinkForSomeBound shrink x
