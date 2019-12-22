{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module STLCC.AST.Generic where

import           Numeric.Natural

----------------------------------------------------------------------
---                            Constants                           ---
----------------------------------------------------------------------
type family GConstFam (tag :: t) (ty :: *) :: *

data GConst (tag :: t)  =
    NatC (GConstFam tag Natural) Natural
  | IntC (GConstFam tag Integer) Integer
  | BoolC (GConstFam tag Bool) Bool


----------------------------------------------------------------------
---                    Expressions, Generalised                    ---
----------------------------------------------------------------------


type family GEAppFam (tag :: t) :: *

type family GELamFam (tag :: t) :: *

type family GELamIxFam (tag :: t) :: t

type family GEVarFam (tag :: t) :: *

type family GEGlobalFam (tag :: t) :: *

type family GEConstFam (tag :: t) :: *

type family GEAnnotFam (tag :: t) :: *

type family GEEmbedFam (tag :: t) :: *

data GExp tag =
    GEApp (GEAppFam tag) (GExp tag) (GExp tag)
  | GELam (GELamFam tag) (GExp (GELamIxFam tag))
  | GEVar (GEVarFam tag)
  | GEGlobal (GEGlobalFam tag)
  | GEConst (GEConstFam tag) (GConst tag)
  | GEAnnot (GEAnnotFam tag) (GExp tag) (GTyExp tag)
  | GEEmbed (GEEmbedFam tag)

----------------------------------------------------------------------
---                       Types, Generalised                       ---
----------------------------------------------------------------------

data PrimTy =
  IntegerTy | BoolTy | NaturalTy


type family GTPrimTyFam (tag :: t) :: *

type family GTArrFam (tag :: t) :: *

type family GTGlobalFam (tag :: t) :: *


data GTyExp tag =
  -- We could have foralls and type application and what-not here, but
  -- this is a simply typed calculus so arrows and simple types will
  -- suffice.

  GTPrimTy (GTPrimTyFam tag) PrimTy
  -- ^ Primitive types

  | GTArr (GTArrFam tag) (GTyExp tag) (GTyExp tag)
  -- ^ Functions

  | GTGlobal (GTGlobalFam tag)
  -- ^ Global type aliases.
