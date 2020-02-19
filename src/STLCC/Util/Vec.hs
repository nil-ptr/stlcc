{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  STLCC.Util.Vec
-- Copyright   :  Nils Gustafsson 2019-2020
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  nils.gustafsson@bredband2.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Length indexed lists.
module STLCC.Util.Vec where

import           Prelude        hiding (map)
import           STLCC.Util.Nat

-- | The good old length indexed list.
--
-- Note that cons (that is '(:::)') is strict in its second
-- argument. No infinite lists here. The elements may be lazy though.
data Vec (s :: *) (n :: Nat) where
  VNil :: Vec s 'Z
  (:::) :: s -> !(Vec s n) -> Vec s ('S n)

infixr 5 :::

-- | Fetch a given element from a 'Vec'. The zeroth element is
-- precisely the head of the 'Vec'.
(!!!) :: Vec s n -> Fin n -> s
VNil       !!! fn             = absurdFinZero fn
(x ::: _)  !!!  FZ            = x
(_ ::: xs) !!! (FS fn@(FS _)) = xs !!! fn
(_ ::: xs) !!! (FS fn@(FZ))   = xs !!! fn
{-# INLINABLE (!!!) #-}

infixl 7 !!!

indexByLTE :: NatLTE m n -> Vec s ('S n) -> s
indexByLTE LTEZero (x ::: _)      = x
indexByLTE (LTESucc n) (_ ::: xs) = indexByLTE n xs
{-# INLINABLE indexByLTE #-}

head :: Vec s ('S n) -> s
head (x ::: _) = x
{-# INLINE head #-}

tail :: Vec s ('S n) -> Vec s n
tail (_ ::: xs) = xs
{-# INLINE tail #-}

-- | Convert a 'Vec' to a regular old lazy list.
vecToList :: Vec s n -> [s]
vecToList VNil       = []
vecToList (x ::: xs) = x : vecToList xs
{-# INLINABLE vecToList #-}

findInVec :: Eq s => s -> Vec s n -> Maybe (Fin n)
findInVec _ VNil = Nothing
findInVec needle (x ::: xs)
  | needle == x = Just FZ
  | otherwise   = case findInVec needle xs of
                    Just fn -> Just (FS fn)
                    Nothing -> Nothing
{-# INLINABLE findInVec #-}


instance Show (Vec a 'Z) where
  show VNil = "VNil"

instance (Show (Vec a n), Show a) => Show (Vec a ('S n)) where
  show (x ::: xs) = show x ++ " ::: " ++ show xs

-- | Analogous to 'Data.List.map'.
map :: (a -> b) -> Vec a n -> Vec b n
map f (x ::: xs) = (f x ::: map f xs)
map _ VNil       = VNil
{-# INLINABLE map #-}

transposeWithList :: Vec [a] n -> [Vec a n]
transposeWithList VNil       = [VNil]
transposeWithList (x ::: xs) = do
  x' <- x
  xs' <- transposeWithList xs
  pure (x' ::: xs')
{-# INLINABLE transposeWithList #-}
