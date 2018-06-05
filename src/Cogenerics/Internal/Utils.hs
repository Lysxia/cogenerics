{-# LANGUAGE
    AllowAmbiguousTypes,
    ConstraintKinds,
    DataKinds,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Cogenerics.Internal.Utils where

type family If (b :: Bool) (x :: k) (y :: k) :: k where
  If 'True   x _y = x
  If 'False _x  y = y

type If' (b :: Bool) x y = (If b x y, IsBool b)

class IsBool (b :: Bool) where
  _If :: forall a. (('True ~ b) => a) -> (('False ~ b) => a) -> a

instance IsBool 'True where
  _If a _ = a

instance IsBool 'False where
  _If _ b = b

-- | "Data.Type.Equality" has another definition of
-- @('Data.Type.Equality.==')@, but I found reflexive equality more useful
-- in type-level programming.
type family a == b where
  a == a = 'True
  a == b = 'False
