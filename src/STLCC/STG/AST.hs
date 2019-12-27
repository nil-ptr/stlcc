{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module STLCC.STG.AST where

import           Data.Text
import           STLCC.Util.Nat
import           STLCC.Util.Vec


----------------------------------------------------------------------
---                            Variables                           ---
----------------------------------------------------------------------

data BindList
     (ty :: *)
     (init :: Nat)
     (ambient :: Nat)
     (capture :: Nat)
     (bind :: Nat)
     (n :: Nat) where
  BNil :: BindList ty i a 'Z 'Z i
  (::>) :: ty -> !(BindList ty i a 'Z b n) -> BindList ty i a 'Z ('S b) ('S n)
  (::<) :: Fin a -> !(BindList ty i a c b n) -> BindList ty i a ('S c) b ('S n)

infixr 5 ::>
infixr 5 ::<

data NP (n :: Nat) = NP


fromBindVec :: Vec ty n -> BindList ty i a 'Z n (NatPlus n i)
fromBindVec (x ::: xs) = x ::> fromBindVec xs
fromBindVec VNil       = BNil

addCapVec :: Vec (Fin a) m
          -> BindList ty i a 'Z n k
          -> BindList ty i a m n (NatPlus m k)
addCapVec (x ::: xs) bl = x ::< addCapVec xs bl
addCapVec VNil       bl = bl

fromVecs :: Vec (Fin a) m
         -> Vec ty n
         -> BindList ty i a m n (NatPlus m (NatPlus n i))
fromVecs vc vb = addCapVec vc (fromBindVec vb)



type LamBinds vTy ambient captured bound inScope =
  BindList vTy 'Z ambient captured bound inScope

data STGLam (vTy :: *) (n :: Nat) where
  Lam :: Bool
      -> LamBinds vTy ambient captured bound inScope
      -> STGExpr vTy inScope
      -> STGLam vTy ambient

type LetBinds vTy ambient bound inScope =
  BindList (vTy, STGLam vTy ambient) ambient ambient 'Z bound inScope

type LetRecBinds vTy ambient bound inScope =
  BindList (vTy, STGLam vTy inScope) ambient ambient 'Z bound inScope

type CaseArmBinds vTy ambient bound inScope =
  BindList vTy ambient ambient 'Z bound inScope

----------------------------------------------------------------------
---                            Patterns                            ---
----------------------------------------------------------------------

-- | Pattern stub.
data STGPattern (bound :: Nat)

data STGCaseArm vTy ambient where
  CaseArm :: CaseArmBinds vTy ambient bound inScope
          -> STGPattern bound
          -> STGExpr vTy inScope
          -> STGCaseArm vTy ambient
  TrivialArm :: vTy
             -> STGExpr vTy ('S ambient)
             -> STGCaseArm vTy ambient
  DefaultArm :: STGExpr vTy ambient
             -> STGCaseArm vTy ambient

----------------------------------------------------------------------
---                               AST                              ---
----------------------------------------------------------------------

-- | Prim op stub.
data STGPrimOp

-- | Constructor stub.
data STGCon

data STGExpr (vTy :: *) (n :: Nat) where
  -- Lam :: Bool                                        -- ^ Updatable?
  --     -> LamBinds vTy ambient captured bound inScope -- ^ Bindings
  --     -> STGExpr vTy inScope                         -- ^ Body
  --     -> STGExpr vTy ambient

  Let :: LetBinds vTy ambient bound inScope          -- ^ Let bindings
      -> STGExpr vTy inScope                         -- ^ Body
      -> STGExpr vTy ambient

  LetRec :: LetRecBinds vTy ambient bound inScope    -- ^ LetRec bindings
         -> STGExpr vTy inScope                      -- ^ Body
         -> STGExpr vTy ambient

  Var :: Fin n -> STGExpr vTy n

  App :: STGExpr vTy n                               -- ^ Function
      -> Vec (STGExpr vTy n) m                       -- ^ Arguments
      -> STGExpr vTy n

  PrimOp :: STGPrimOp -> STGExpr vTy n
  Con :: STGCon -> STGExpr vTy n

  Case :: STGExpr vTy n                              -- ^ Scrutinee
       -> Vec (STGCaseArm vTy n) m                   -- ^ Arms
       -> STGExpr vTy n

foo :: STGLam Text ('S ('S 'Z))
foo = Lam True
      (FS FZ ::< ("a" :: Text) ::> "b" ::> BNil)
      (Var FZ)


feh :: STGExpr Text 'Z
feh = LetRec (("ff", foo) ::> ("b", Lam True (FS FZ ::< BNil) (Var FZ))::> BNil) (Var (FS FZ))
