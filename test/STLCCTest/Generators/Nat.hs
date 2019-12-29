{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeApplications, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Generators.Nat
-- Copyright   :  Nils Gustafsson 2019
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan instance ahead. Specifically this module defines an
-- 'Arbitrary' instance for 'Fin'.
module STLCCTest.Generators.Nat where


import  STLCC.Util.Nat
import  Test.QuickCheck


instance KnownSNat n => Arbitrary (Fin ('S n)) where
  arbitrary = do
    let maxval :: Int
        maxval = fromInteger . toInteger . snatToNatural $ snat @n
    x <- choose (0, maxval)
    pure (reduceFinBy x maxFin)

  shrink fn =
    let minval :: Int
        minval = fromInteger . toInteger . finToNatural $ fn
    in flip reduceFinBy fn <$> enumFromThenTo (minval-1) (minval - 2) 0
