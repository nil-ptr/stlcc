{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  STLCCTest.Test.Util.Nat
-- Copyright   :  Nils Gustafsson 2019-2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Inductively defined typelevel natural numbers, and types for
-- interacting with them.
module STLCC.Util.Nat where


import           Numeric.Natural

-- For a specific RULES definition
import qualified Unsafe.Coerce   as Coerce (unsafeCoerce)


----------------------------------------------------------------------
---                           Index Types                          ---
----------------------------------------------------------------------

--- Nats and Fin -----------------------------------------------------

-- | Inductively defined natural numbers. Intended for use at the type
-- level only.
data Nat = Z | S !Nat

-- | Type level addition for 'Nat's, defined by matching on the first
-- argument. Additionally, there is an equation witnessing
--
-- @
--   NatPlus x 'Z = x
-- @
-- .
type family NatPlus (m :: Nat) (n :: Nat) :: Nat where
  NatPlus 'Z y = y
  -- This eq needs undecidable instances
  NatPlus ('S x) y = 'S (NatPlus x y)
  -- Important: This eq *must* be defined last, so that we match on
  -- the second argument as late as possible.
  NatPlus x 'Z = x

-- | A natural number guaranteed to be less than a given
-- typelevel bound.
data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: !(Fin n) -> Fin ('S n)

-- | Derived instance.
deriving instance Show (Fin n)

-- | Derived instance.
deriving instance Eq (Fin n)

-- | The 'Fin' promise is that it's isomorphic to some 'SNat' whose
-- index is /strictly/ less than the given index of the given
-- 'Fin'. No 'SNat' could have a 'Nat' kinded index strictly smaller
-- than zero.
absurdFinZero :: Fin 'Z -> a
absurdFinZero x =
  case x of {}



--- Singletons -------------------------------------------------------

-- | Nat Singletons.
data SNat (n :: Nat) where
  -- Avoiding SZ and SS here cause of the possible confusion with (S Z
  -- :: Nat), and (S . S :: Nat -> Nat).
  --
  -- Not super confusing in practice, probably, but I don't like it.
  SZero :: SNat 'Z
  SSucc :: !(SNat n) -> SNat ('S n)

-- | For a given typelevel 'Nat', there is a unique singleton 'snat'.
class KnownSNat (n :: Nat) where
  snat :: SNat n

instance KnownSNat 'Z where
  snat = SZero
  {-# INLINE snat #-}

instance KnownSNat n => KnownSNat ('S n) where
  snat = SSucc snat
  {-# INLINE snat #-}


----------------------------------------------------------------------
---                      Conversion Functions                      ---
----------------------------------------------------------------------


-- | Build a 'Fin' given an 'SNat'. The extra 'S' in the result is
-- there because the guarantee that 'Fin' offers is that it's value is
-- /strictly/ less than it's type index.
snatToFin :: SNat n -> Fin ('S n)
snatToFin SZero      = FZ
snatToFin (SSucc sn) = FS (snatToFin sn)
{-# INLINABLE snatToFin #-}

-- | The largest possible value of a 'Fin' at a given index. This is
-- just @snatToFin snat@.
maxFin :: KnownSNat n => Fin ('S n)
maxFin = snatToFin snat
{-# INLINE maxFin #-}

-- | Convert an 'SNat' to a 'Natural' of the same magnitude.
--
-- That is 'SZero' maps to @0@, and 'SSucc' maps to @(+ 1)@.
snatToNatural :: SNat n -> Natural
snatToNatural SZero     = 0
snatToNatural (SSucc n) = 1 + snatToNatural n
{-# INLINABLE snatToNatural #-}

-- | Like 'snatToNatural', but with a 'Fin' argument.
finToNatural :: Fin n -> Natural
finToNatural FZ     = 0
finToNatural (FS n) = 1 + finToNatural n
{-# INLINABLE finToNatural #-}


----------------------------------------------------------------------
---                        Utility Functions                       ---
----------------------------------------------------------------------

-- | Increases the index of a 'Fin' without altering it's value. In
-- other words, this function \"weakens\" the 'Fin', by relaxing the
-- bound it advertises.
incrFin :: Fin n -> Fin ('S n)
incrFin FZ     = FZ
incrFin (FS x) = FS (incrFin x)
-- This function does no _useful_ work. All it's really doing is
-- changing the type of the argument. The type itself doesn't exist at
-- runtime though, so let's make sure this function is never around at
-- runtime either.
{-# NOINLINE incrFin #-}
{-# RULES "incrFin/nop" forall x. incrFin x = Coerce.unsafeCoerce x #-}

-- | Given a proof that @m@ is less than or equal to @n@, increase the
-- bound on a 'Fin' from @m@ to @n@.
incrFinTo :: NatLTE m n -> Fin ('S m) -> Fin ('S n)
incrFinTo _ FZ                  = FZ
incrFinTo LTEZero (FS fn)       = absurdFinZero fn
incrFinTo (LTESucc ltp) (FS fn) = FS (incrFinTo ltp fn)
-- This function is a do nothing function for much the same reasons as
-- incrFin. So we can arguably replace it with unsafeCoerce for the
-- same reasons.
{-# NOINLINE incrFinTo #-}
{-# RULES "incrFinTo/nop" forall p x. incrFinTo p x = Coerce.unsafeCoerce x #-}


-- | Reduces a 'Fin' by stripping off 'FS' constructors. The first
-- argument dictates how many constructors to remove. A negative
-- argument is treated as 0.
reduceFinBy :: Integral a => a -> Fin n -> Fin n
reduceFinBy _ FZ = FZ
reduceFinBy x (FS ff) | x > 0 = incrFin (reduceFinBy (x - 1) ff)
                      | otherwise = (FS ff)
{-# INLINABLE reduceFinBy #-}

-- | Enumerate every valid 'Fin' value up to, and including, the given
-- bound.
enumFinTo :: Fin n -> [Fin n]
enumFinTo mf = go mf []
  where go :: Fin n -> [Fin n] -> [Fin n]
        go f@(FS fn) l = go (incrFin fn) (f:l)
        go FZ l        = FZ : l
{-# INLINABLE enumFinTo  #-}

----------------------------------------------------------------------
---                   Less Than Or Equal Evidence                  ---
----------------------------------------------------------------------

-- | Witness that one 'Nat', @m@, is less than or equal to another, @n@.
data NatLTE (m :: Nat) (n :: Nat) where
  LTEZero :: NatLTE 'Z n
  -- ^ Zero is less than or equal to all 'Nat's.
  LTESucc :: !(NatLTE m n) -> NatLTE ('S m) ('S n)
  -- ^ Congruence.

natLTEtoFin :: NatLTE m n -> Fin ('S n)
natLTEtoFin LTEZero     = FZ
natLTEtoFin (LTESucc n) = FS (natLTEtoFin n)
{-# INLINABLE natLTEtoFin #-}

-- | Anything that reduces to a proof that some non-zero natural
-- number is less than or equal to zero is clearly absurd.
absurdNatLTE :: NatLTE ('S n) 'Z -> a
absurdNatLTE x =
  case x of {}

----------------------------------------------------------------------
---                      Existentials For Nats                     ---
----------------------------------------------------------------------

-- | Existential quantification for 'Nat' kinded arguments.
data ForSomeNat (t :: Nat -> *) where
  MkForSomeNat :: KnownSNat n => t n -> ForSomeNat t

-- | Existential quantification with a proof that the existentially
-- quantified 'Nat' kinded variable is bounded by some 'Nat' @n@, whose
-- value is known.
data ForSomeNatLTE (n :: Nat) (t :: Nat -> *) where
  MkForSomeNatLTE :: NatLTE m n -> t m -> ForSomeNatLTE n t


--- fromNatural style functions --------------------------------------

-- | Construct a 'SNat' from a 'Natural'.
--
-- @
--   snatFromNatural (snatToNatural x) = MkForSomeNat x
-- @
snatFromNatural :: Natural -> ForSomeNat SNat
snatFromNatural n | n > 0 = case snatFromNatural (n-1) of
                              MkForSomeNat sn -> MkForSomeNat (SSucc sn)
                  | otherwise = MkForSomeNat SZero
{-# INLINABLE snatFromNatural #-}


-- | Construct a 'Fin' from a 'Natural'
--
-- @
--   finFromNatural (finToNatural x) = MkForSomeNat x
-- @
finFromNatural :: Natural -> ForSomeNat Fin
finFromNatural n | n > 0 = case finFromNatural (n - 1) of
                             MkForSomeNat fn -> MkForSomeNat (FS fn)
                 | otherwise = MkForSomeNat @('S 'Z) FZ
{-# INLINABLE finFromNatural #-}

----------------------------------------------------------------------
---               General SNat to/from Fin Conversion              ---
----------------------------------------------------------------------


snatFinForward :: ForSomeNatLTE n SNat -> Fin ('S n)
snatFinForward (MkForSomeNatLTE p sn) = incrFinTo p (snatToFin sn)

snatFinBackward :: Fin ('S n) -> ForSomeNatLTE n SNat
snatFinBackward FZ = MkForSomeNatLTE LTEZero SZero
snatFinBackward (FS FZ) = MkForSomeNatLTE (LTESucc LTEZero) (SSucc SZero)
snatFinBackward (FS fn@(FS _)) =
  case snatFinBackward fn of
    MkForSomeNatLTE p sn -> MkForSomeNatLTE (LTESucc p) (SSucc sn)
