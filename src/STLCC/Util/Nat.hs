{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module STLCC.Util.Nat where


import           Numeric.Natural

----------------------------------------------------------------------
---                           Index Types                          ---
----------------------------------------------------------------------

--- Nats and Fin -----------------------------------------------------

-- | Inductively defined natural numbers. Intended for use at the type
-- level only.
data Nat = Z | S !Nat

-- | Proof that a given typelevel natural is smaller than some known
-- (or knowable) natural number.
data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: !(Fin n) -> Fin ('S n)

deriving instance Show (Fin n)

-- | Nat Singletons.
data SNat (n :: Nat) where
  -- Avoiding SZ and SS here cause of the possible confusion with (S Z
  -- :: Nat), and (S . S :: Nat -> Nat).
  --
  -- Not super confusing in practice, probably, but I don't like it.
  SZero :: SNat 'Z
  SSucc :: !(SNat n) -> SNat ('S n)


class KnownSNat (n :: Nat) where
  snat :: SNat n

instance KnownSNat 'Z where
  snat = SZero
  {-# INLINE snat #-}
instance KnownSNat n => KnownSNat ('S n) where
  snat = SSucc snat
  {-# INLINE snat #-}

snatToNatural :: SNat n -> Natural
snatToNatural SZero     = 0
snatToNatural (SSucc n) = 1 + snatToNatural n
{-# INLINABLE snatToNatural #-}

finToNatural :: Fin n -> Natural
finToNatural FZ     = 0
finToNatural (FS n) = 1 + finToNatural n
{-# INLINABLE finToNatural #-}