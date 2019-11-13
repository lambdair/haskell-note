{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel.Vec
    (
    Vec(..),
    vecLength,
    vecZip,
    vecConcat
    ) where

import TypeLevel.Nat


data Vec (n :: Nat) a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a

instance Show a => Show (Vec n a) where
    show Nil = "Nil"
    show (Cons a va) = show a ++ ":" ++ show va


vecLength :: Vec n a -> Nat
vecLength Nil = Zero
vecLength (Cons a va) = Succ (vecLength va)

vecZip :: Vec n a -> Vec m b -> Vec (Min n m) (a, b)
vecZip Nil Nil = Nil
vecZip (Cons _ Nil) Nil = Nil
vecZip Nil (Cons _ Nil) = Nil
vecZip (Cons a va) (Cons b vb) = Cons (a, b) (vecZip va vb)

vecConcat :: Vec n a -> Vec m a -> Vec (Add n m) a
vecConcat Nil Nil = Nil
vecConcat Nil vb = vb
vecConcat (Cons a va) vb = Cons a (vecConcat va vb)