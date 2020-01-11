{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Test.Util.Nat
-- Copyright   :  Nils Gustafsson 2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the "STLCC.Util.Nat" module.
module STLCCTest.Test.Util.Nat
  (
    natSpec
  ) where


import           Test.Hspec
import           Test.QuickCheck

import           STLCCTest.Generators.Nat ()

import           STLCC.Util.Nat


----------------------------------------------------------------------
---                            Nat Spec                            ---
----------------------------------------------------------------------

-- | 'Spec' for the "STLCC.Util.Nat" module.
natSpec :: Spec
natSpec = describe "STLCC.Util.Nat" finSpec


----------------------------------------------------------------------
---                            Fin Spec                            ---
----------------------------------------------------------------------

type NatOne = 'S 'Z

type NatFive = 'S ('S ('S ('S ('S ('Z)))))

type NatTen = NatPlus NatFive NatFive

type NatEleven = NatPlus NatOne NatTen


-- | Do nothing pattern intended to monomorphise the type of the 'Fin'
-- matched.
pattern FinTen :: Fin NatTen -> Fin NatTen
pattern FinTen x = x

-- | Do nothing pattern intended to monomorphise the type of the 'Fin'
-- matched.
pattern FinEleven :: Fin NatEleven -> Fin NatEleven
pattern FinEleven x = x

-- | Spec for the 'Fin' type and associated functions. This is the
-- main piece of functionality in the module.
finSpec :: Spec
finSpec = do
  describe "types" $
    describe "Fin" $ do
      it "contains no values gte to its typelevel bound" $
        property $ \(FinTen x) ->
                     finToNatural x < 10

  describe "functions" $ do
    describe "maxFin" $ do
      it "equals the typlevel bound minus 1" $
        finToNatural (FinEleven maxFin)
        `shouldBe`
        (snatToNatural (snat @NatTen))

      it "is gte to all other values of the same type" $
        property $ \(FinTen x) ->
                     finToNatural x <= finToNatural (FinTen maxFin)

    describe "incrFin" $ do
      it "does not change the value of term it is applied to" $
        property $ \(FinTen x) ->
                     finToNatural x == finToNatural (incrFin x)

    describe "enumFinTo" $ do
      it "produces a list with length equal to the value of its argument + 1" $
        property $ \(FinTen x) ->
                     length (enumFinTo x)
                     ==
                     (fromInteger . toInteger)
                     (1 + finToNatural x)

      it "the result contains the argument as the last element" $
        property $ \(FinTen x) ->
                     x == last (enumFinTo x)

      it "produces an ordered list of all distinct Fins <= the argument" $
        property $ \(FinTen x) ->
                     let ls = enumFinTo x

                         ls' = finToNatural <$> ls

                         -- If ls' is a list of the form [0,1,2, ..]
                         -- then lz is a list of the form
                         -- [(0,1),(1,2), ...].
                         --
                         -- Consequently, if ls' was strictly ordered,
                         -- the lz should contain only pairs of values
                         -- where the first element is strictly less
                         -- than the second.


                         lz = zip (init ls') (tail ls')
                     in
                       -- Strict Ordering holds. Therefore the list is
                       -- ordered _and_ there are no duplicate elements.
                       all (uncurry (<)) lz

                       -- We could also check that every element is
                       -- actually <= the argument, but that follows
                       -- from the strict ordering, combined with the
                       -- following:
                       &&
                       x == last ls

    describe "reduceFinBy" $ do

      it "does nothing, given a negative argument" $
        property $ \(FinTen x) (Negative i) ->
                     reduceFinBy @Int i x == x

      it "for a positive n: 'reduceFinBy n x' equals the nth pred of x" $
        let fspred :: Fin ('S ('S n)) -> Fin ('S n)
            fspred FZ      = FZ
            fspred (FS fn) = fn

            repeatFsPred :: Int -> Fin ('S ('S n)) -> Fin ('S ('S n))
            repeatFsPred i x | i > 0 = repeatFsPred (i - 1) (incrFin (fspred x))
                             | otherwise = x


        in property $ \(FinTen x)
                       -- The use of small here is intended to reduce
                       -- the odds of a test being redundant.
                       (Positive (Small i)) ->
                        reduceFinBy i x == repeatFsPred i x
