{-# LANGUAGE
    DataKinds,
    LambdaCase,
    PartialTypeSignatures,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    ViewPatterns #-}

import Cogenerics.Internal.Core

type E_ a b =
  C "L" (a :* ()) :+
  C "R" (b :* ()) :+
  V

type E a b = Per (E_ a b)

-- Construction
l :: a -> E a b
l a = D (C (a :* ()) :: C "L" _)

r :: b -> E a b
r b = D (C (b :* ()) :: C "R" _)

-- Destruction
e :: (a -> r) -> (b -> r) -> E a b -> r
e f g = \case
  D (C (a :* ()) :: C "L" _) -> f a
  D (C (b :* ()) :: C "R" _) -> g b
  _ -> error "impossible"

e' :: (a -> r) -> (b -> r) -> E a b -> r
e' f g = \case
  D (unC @"L" -> (a :* ())) -> f a
  D (unC @"R" -> (b :* ())) -> g b
  _ -> error "impossible"

main :: IO ()
main = do
  print (l 0 :: E Int String)
  print (r "pikachu" :: E Int String)
  print (e  (+ 1) length (l 0 :: E Int String))
  print (e' (+ 1) length (l 0 :: E Int String))
