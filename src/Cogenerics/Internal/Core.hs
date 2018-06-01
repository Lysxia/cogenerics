{-# LANGUAGE
    AllowAmbiguousTypes,
    BangPatterns,
    ConstraintKinds,
    DataKinds,
    DeriveFunctor,
    FlexibleContexts,
    FlexibleInstances,
    LambdaCase,
    MultiParamTypeClasses,
    PatternSynonyms,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances,
    ViewPatterns #-}

module Cogenerics.Internal.Core where

import GHC.TypeLits

import Cogenerics.Internal.Utils

-- | A data type defined by its generic representation.
--
-- Palindrome of 'GHC.Generics.Rep'.
newtype Per a = Per { unPer :: a }
  deriving Show

-- | Sum of constructors.
--
-- @
-- C x1 a1 :+ (C x2 a2 :+ ... (... :+ ()) ...)
-- @
data a :+ b = L a | R b
  deriving (Functor, Show)

infixr 1 :+

unplus :: (a -> r) -> (b -> r) -> a :+ b -> r
unplus f _ (L a) = f a
unplus _ g (R b) = g b

-- | Constructor.
newtype C (name :: Symbol) a = C { unC :: a }
  deriving Show

data V

absurd :: V -> a
absurd !_ = error "impossible"

instance Show V where
  show = absurd

-- | Product of fields.
data a :* b = a :* b
  deriving Show

infixr 2 :*

-- | Field.
newtype F a = F a

-- | Match a constructor.
class Match (n :: Symbol) a s t where
  match :: s -> a :+ t

instance
  ( If' (n == n') c1 c2
  , c1 ~ (a ~ a', s ~ t)
  , c2 ~ (Match n a s t', t ~ (C n' a' :+ t'))
  ) => Match n a (C n' a' :+ s) t where
  match =
    _If @(n == n')
      @c1
      (\case
        L (C a) -> L a
        R s -> R s)
      @c2
      (\case
        L b -> R (L b)
        R s -> case match @n s of
          L a -> L a
          R b -> R (R b))

-- | Construct a variant.
class Inject (n :: Symbol) a s where
  inject :: a -> s

instance
  ( If' (n == n') c1 c2
  , c1 ~ (a ~ a')
  , c2 ~ Inject n a s
  ) => Inject n a (C n' a' :+ s) where
  inject =
    _If @(n == n')
      @c1
      (L . C)
      @c2
      (R . inject @n)

-- | Generic data constructor.
pattern D :: forall n a s t. (Inject n a s, Match n a s t) => C n a -> Per s
pattern D c <- Per (match @n @a @s @t -> L (C -> c))
  where D (C a) = Per (inject @n @a @s a)

c :: forall n a s. Inject n a s => a -> Per s
c a = Per (inject @n @a @s a)
