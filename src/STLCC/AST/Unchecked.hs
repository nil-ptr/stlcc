{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
module STLCC.AST.Unchecked where

import           Data.Text
import           Data.Void
import           Numeric.Natural
import           STLCC.AST.Generic
import           STLCC.Util.Nat
import           STLCC.Util.Vec


----------------------------------------------------------------------
---                               Tag                              ---
----------------------------------------------------------------------

newtype Unchecked = Unchecked Nat

----------------------------------------------------------------------
---                            Constants                           ---
----------------------------------------------------------------------

type UConst n = GConst ('Unchecked n)

type instance GConstFam ('Unchecked n) Natural = ()
type instance GConstFam ('Unchecked n) Integer = ()
type instance GConstFam ('Unchecked n) Bool    = ()

pattern UNatC :: Natural -> UConst n
pattern UNatC n = NatC () n

pattern UIntC :: Integer -> UConst n
pattern UIntC n = IntC () n

pattern UBoolC :: Bool -> UConst n
pattern UBoolC n = BoolC () n


----------------------------------------------------------------------
---                   Unchecked Type Expressions                   ---
----------------------------------------------------------------------

type UTyExp (n :: Nat) = GTyExp ('Unchecked n)

-- Nothing to tag with here

type instance GTPrimTyFam ('Unchecked n) = ()
type instance GTArrFam    ('Unchecked n) = ()
type instance GTGlobalFam ('Unchecked n) = Text

pattern UNatTy :: UTyExp n
pattern UNatTy = GTPrimTy () NaturalTy

pattern UIntTy :: UTyExp n
pattern UIntTy = GTPrimTy () IntegerTy

pattern UBoolTy :: UTyExp n
pattern UBoolTy = GTPrimTy () BoolTy

pattern UArrTy :: UTyExp n -> UTyExp n -> UTyExp n
pattern UArrTy dom codom = GTArr () dom codom

pattern UAliasTy :: Text -> UTyExp n
pattern UAliasTy s = GTGlobal s

{-# Complete UNatTy
           , UIntTy
           , UBoolTy
           , UArrTy
           , UAliasTy #-}

instance Eq (UTyExp n) where
  UNatTy == UNatTy = True
  UIntTy == UIntTy = True
  UBoolTy == UBoolTy = True
  (UAliasTy a) == (UAliasTy b) = a == b
  (UArrTy da cda) == (UArrTy db cdb)= da == db
                                      && cda == cdb
  _ == _ = False

utyexpSize :: UTyExp n -> Natural
utyexpSize = go 0
  where go :: Natural -> UTyExp k -> Natural
        go a UNatTy        = 1 + a
        go a UIntTy        = 1 + a
        go a UBoolTy       = 1 + a
        go a (UAliasTy _)  = 1 + a
        go a (UArrTy d cd) = go (go (1 + a) d) cd

----------------------------------------------------------------------
---                      Unchecked Expressions                     ---
----------------------------------------------------------------------


type UExp (n :: Nat) = GExp ('Unchecked n)

type instance GEAppFam    ('Unchecked n) = ()
type instance GELamFam    ('Unchecked n) = (Text, Maybe (UTyExp n))
type instance GELamIxFam  ('Unchecked n) = 'Unchecked ('S n)
type instance GEVarFam    ('Unchecked n) = Fin n
type instance GEGlobalFam ('Unchecked n) = Text
type instance GEConstFam  ('Unchecked n) = ()
type instance GEAnnotFam  ('Unchecked n) = ()

-- No embeded constructors in this case.
type instance GEEmbedFam  ('Unchecked n) = Void

pattern UApp :: UExp n -> UExp n -> UExp n
pattern UApp l r = GEApp () l r

pattern ULam :: Text -> Maybe (UTyExp n) -> UExp ('S n) -> UExp n
pattern ULam s ty e = GELam (s, ty) e

pattern UVar :: Fin n -> UExp n
pattern UVar ix = GEVar ix

pattern UGlobal :: Text -> UExp n
pattern UGlobal s = GEGlobal s

pattern UAnnot :: UExp n -> UTyExp n -> UExp n
pattern UAnnot term ty = GEAnnot () term ty

pattern UNatE :: Natural -> UExp n
pattern UNatE n = GEConst () (UNatC n)

pattern UIntE :: Integer -> UExp n
pattern UIntE n = GEConst () (UIntC n)

pattern UBoolE :: Bool -> UExp n
pattern UBoolE n = GEConst () (UBoolC n)

{-# Complete UApp
             , ULam
             , UVar
             , UGlobal
             , UAnnot
             , UNatE
             , UIntE
             , UBoolE
             #-}

instance Eq (UExp n) where
  (UBoolE a) == (UBoolE b) = a == b
  (UIntE a) == (UIntE b) = a == b
  (UNatE a) == (UNatE b) = a == b
  (UAnnot ta tya) == (UAnnot tb tyb) = ta == tb
                                       && tya ==  tyb
  (UGlobal a) == (UGlobal b) = a == b
  (UVar fna) == (UVar fnb) = fna == fnb
  (ULam sa tya ea) == (ULam sb tyb eb) = sa == sb
                                         && tya == tyb
                                         && ea ==  eb
  (UApp la ra) == (UApp lb rb) = la == lb
                                 && ra == rb
  _ == _  = False


uexpSize :: UExp n -> Natural
uexpSize = go 0
  where go :: Natural -> UExp k -> Natural
        go a (UApp l r)    = go (1 + go a l) r
        go a (ULam _ t e)  = go (1 + a + mbe utyexpSize t) e
        go a (UVar _)      = 1 + a
        go a (UBoolE _)    = 1 + a
        go a (UIntE _)     = 1 + a
        go a (UNatE _)     = 1 + a
        go a (UGlobal _)   = 1 + a
        go a (UAnnot t ty) = go (1 + a + utyexpSize ty) t

        mbe _ Nothing  = 0
        mbe f (Just t) = f t

----------------------------------------------------------------------
---                            Printing                            ---
----------------------------------------------------------------------

instance Show (GExp ('Unchecked n)) where

  showsPrec d (UApp l r) = showParen (d > 10) $
    showString "UApp "
    . showsPrec 11 l
    . showString " "
    . showsPrec 11 r
  showsPrec d (ULam s _ r) = showParen (d > 10) $
    showString "ULam "
    . showsPrec 11 s
    . showString " _ "
    . showsPrec 11 r
  showsPrec d (UVar fn) = showParen (d > 10) $
    showString "UVar " . showsPrec 11 fn
  showsPrec d (UGlobal s) = showParen (d > 10) $
    showString "UGlobal \""
    . showsPrec 11 s
    . showString "\""
  showsPrec d (UAnnot t _) = showParen (d > 10) $
    showString "UAnnot "
    . showsPrec 11 t
    . showString " _"
  showsPrec d (UNatE n) = showParen (d > 10) $
    showString "UNatE "
    . showsPrec 11 n
  showsPrec d (UIntE i) = showParen (d > 10) $
    showString "UIntE "
    . showsPrec 11 i
  showsPrec d (UBoolE b) = showParen (d > 10) $
    showString "UBoolE "
    . showsPrec 11 b

pretty :: UExp n -> String
pretty (UApp l r@(UApp _ _)) = pretty l ++ " (" ++ pretty r ++ ")"
pretty (UApp l r)   = pretty l ++ " " ++ pretty r
pretty (ULam s _ e) = "(λ" ++ unpack s ++
                    ". " ++ pretty e ++ ")"
pretty (UVar fn)    = "#" ++ show (finToNatural fn)
pretty (UGlobal s)  = "«" ++ unpack s ++ "»"
pretty (UAnnot t _) = pretty t
pretty (UNatE n)    = show n
pretty (UIntE i)    = show i
pretty (UBoolE b)   = show b

prettyNamed :: UExp 'Z -> String
prettyNamed (UApp l r@(UApp _ _)) = prettyNamed l ++ " (" ++ prettyNamed r ++ ")"
prettyNamed (UApp l r)   = prettyNamed l ++ " " ++ prettyNamed r
prettyNamed (ULam s _ e) = "(λ" ++ unpack s ++
                    ". " ++ prettyOpenNamed (s:::VNil) e ++ ")"
prettyNamed (UVar fn)    = "#" ++ show (finToNatural fn)
prettyNamed (UGlobal s)  = unpack s
prettyNamed (UAnnot t _) = prettyNamed t
prettyNamed (UNatE n)    = show n
prettyNamed (UIntE i)    = show i
prettyNamed (UBoolE b)   = show b

prettyOpenNamed :: Vec Text ('S n) -> UExp ('S n) -> String
prettyOpenNamed v (UApp l r@(UApp _ _)) =
  prettyOpenNamed v l ++ " (" ++
  prettyOpenNamed v r ++ ")"
prettyOpenNamed v (UApp l r)   =
  prettyOpenNamed v l ++ " " ++
  prettyOpenNamed v r
prettyOpenNamed v (ULam s _ e) =
  "(λ" ++ unpack s ++ ". " ++
  prettyOpenNamed (s:::v) e ++ ")"
prettyOpenNamed v (UVar fn)    =
  unpack (v !!! fn)
prettyOpenNamed _ (UGlobal s)  = unpack s
prettyOpenNamed v (UAnnot t _) = prettyOpenNamed v t
prettyOpenNamed _ (UNatE n)    = show n
prettyOpenNamed _ (UIntE i)    = show i
prettyOpenNamed _ (UBoolE b)   = show b



-- (\x. plus x 1)

example :: UExp 'Z
example = ULam "x" Nothing (UApp (UApp (UGlobal "plus") (UVar FZ )) (UNatE 1))


-- (\x. (\y : Natural. plus x (plus 1 y)))

example2 :: UExp 'Z
example2 =
  ULam "x" Nothing
  (ULam "y" (Just UNatTy)
    (UApp (UApp (UGlobal "plus") (UVar (FS FZ)))
      (UApp (UApp (UGlobal "plus") (UNatE 1)) (UVar FZ))))
