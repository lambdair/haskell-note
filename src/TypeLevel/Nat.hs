{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel.Nat
    (
    Nat(..),
    SNat(..),
    Min,
    Max,
    Add,
    Subtract,
    Multiply,
    zero,
    one,
    two,
    three,
    four,
    five
    ) where

-- Natural numbers and zero
data Nat = Zero | Succ Nat
    deriving Show


-- Singleton
data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

instance Show (SNat a) where
    show SZero = "Zero"
    show (SSucc n) = "Succ (" ++ show n ++ ")"


class Singleton (n :: Nat) where
    sing :: SNat n

instance Singleton Zero where
    sing = SZero

instance Singleton n => Singleton (Succ n) where
    sing = SSucc sing


-- Type families
type family Min (n :: Nat) (m :: Nat) where
    Min Zero m = Zero
    Min n Zero = Zero
    Min (Succ n) (Succ m) = Succ (Min n m)

type family Max (n :: Nat) (m :: Nat) where
    Max Zero m = m
    Max n Zero = n
    Max (Succ n) (Succ m) = Succ (Max n m)

type family Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Add n (Succ m)

type family Subtract (n :: Nat) (m :: Nat) :: Nat
type instance Subtract n Zero = n
type instance Subtract (Succ n) (Succ m) = Subtract n m

type family Multiply (n :: Nat) (m :: Nat) :: Nat
type instance Multiply Zero m = Zero
type instance Multiply (Succ Zero) m = m
type instance Multiply (Succ (Succ n)) m = Add m (Multiply (Succ n) m)


-- Type level numbers
type Zero' = Zero
type One' = Succ Zero
type Two' = Succ (Succ Zero)

zero :: SNat Zero'
zero = sing

one :: SNat One'
one = sing

two :: SNat (Add One' One')
two = sing

three :: SNat (Add One' Two')
three = sing

four :: SNat (Multiply Two' Two')
four = sing

five :: SNat (Add Two' (Add Two' One'))
five = sing