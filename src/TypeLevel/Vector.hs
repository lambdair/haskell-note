{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel.Vector
    (
    Vector(..),
    vectorZip
    ) where

import TypeLevel.Nat


data Vector (n :: Nat) a where
    Nil :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
    show Nil = "Nil"
    show (Cons a va) = show a ++ ":" ++ show va


vectorZip :: Vector m a -> Vector n b -> Vector (Min n m) (a, b)
vectorZip Nil Nil = Nil
vectorZip (Cons _ Nil) Nil = Nil
vectorZip Nil (Cons _ Nil) = Nil
vectorZip (Cons a va) (Cons b vb) = Cons (a, b) (vectorZip va vb)