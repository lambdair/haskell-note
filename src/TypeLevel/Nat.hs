{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel.Nat
    (
    -- Data
    Nat(..),
    SNat(..),

    -- Type families
    Min,
    Max,
    Add,
    Sub,
    Mul,

    -- Functions
    nat2Int,
    add,
    sub,
    mul,

    -- Type level numbers
    _0,
    _1,
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,
    _10,
    _20
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
-- type instance Add (Succ n) m = Add n (Succ m)
type instance Add (Succ n) m = Succ (Add n m)

type family Sub (n :: Nat) (m :: Nat) :: Nat
type instance Sub n Zero = n
type instance Sub (Succ n) (Succ m) = Sub n m

type family Mul (n :: Nat) (m :: Nat) :: Nat
type instance Mul Zero m = Zero
type instance Mul (Succ Zero) m = m
type instance Mul (Succ (Succ n)) m = Add m (Mul (Succ n) m)


nat2Int :: SNat n -> Int
nat2Int SZero = 0
nat2Int (SSucc n) = 1 + nat2Int n

add :: SNat n -> SNat m -> SNat (Add n m)
add SZero m = m
add (SSucc n) m = SSucc (add n m)

sub :: SNat n -> SNat m -> SNat (Sub n m)
sub n SZero = n
sub (SSucc n) (SSucc m) = sub n m

mul :: SNat n -> SNat m -> SNat (Mul n m)
mul SZero m = SZero
mul (SSucc SZero) m = m
mul (SSucc (SSucc n)) m = add m (mul (SSucc n) m)


-- Type level numbers
type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine = Succ Eight
type Ten = Succ Nine

_0 :: SNat Zero
_0 = sing

_1 :: SNat One
_1 = sing

_2 :: SNat (Add One One)
_2 = sing

_3 :: SNat (Add One Two)
_3 = sing

_4 :: SNat (Mul Two Two)
_4 = sing

_5 :: SNat (Add Two (Add Two One))
_5 = sing

_6 :: SNat Six
_6 = sing

_7 :: SNat Seven
_7 = sing

_8 :: SNat Eight
_8 = sing

_9 :: SNat Nine
_9 = sing

_10 :: SNat Ten
_10 = sing

_20 :: SNat (Add (Mul Two Five) (Add Three Seven))
_20 = sing