{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

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

import           STLCC.Util.Nat
import           Test.QuickCheck

instance KnownSNat n =>  Arbitrary (Fin ('S n)) where
  arbitrary = do
    let maxval :: Int
        maxval = fromInteger . toInteger . snatToNatural $ snat @n
    x <- choose (0, maxval)
    pure (reduceFinBy (x-1) maxFin)

  shrink FZ      = []
  shrink (FS fn) = enumFinTo (incrFin fn)
