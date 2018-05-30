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
  _If :: forall c1 c2 a. (c1 => a) -> (c2 => a) -> (If b c1 c2) => a

instance IsBool 'True where
  _If a _ = a

instance IsBool 'False where
  _If _ b = b

type family a == b where
  a == a = 'True
  a == b = 'False