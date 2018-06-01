{-# LANGUAGE
    DataKinds,
    PartialTypeSignatures,
    TypeOperators #-}

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

main :: IO ()
main = do
  print (l 0 :: E Int String)
  print (r "pikachu" :: E Int String)
