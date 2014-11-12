{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Int.Fixed
    ( -- * Generalized fixed-width
      Int'

      -- * Haskell 2010 fixed-width types (re-exported from "Data.Int")
    , Int8, Int16, Int32, Int64

      -- * Internals
    , Int'Ctx

    ) where

import Control.DeepSeq
import Data.Bits
import Data.Ix
import Data.Type.Bool
import Data.Typeable
import Data.Int
import GHC.Arr (Ix(unsafeIndex))
import GHC.Enum
import GHC.TypeLits

-- | Map to a signed integer type of at least the requested bit-size
type IntLeast n = If (n <=? 8)  Int8  (
                  If (n <=? 16) Int16 (
                  If (n <=? 32) Int32 (
                  If (n <=? 64) Int64 (
                  Integer))))

-- | Generalized 'Int' indexed by bit-width @/n/@ with range [-2ⁿ⁻¹, 2ⁿ⁻¹-1].
--
-- Values of the type @'Int'' /n/@ implement the same semantics as the
-- fixed-width signed integer from "Data.Int" but for general
-- bit-widths.
--
-- See documentation of 'Word'' for additional details.

newtype Int'  n = Int' (IntLeast n)
                deriving Typeable

bitSizeInt' :: KnownNat n => Int' n -> Int
bitSizeInt' = fromIntegral . natVal

mkInt' :: (Integral a, Int'Ctx n) => a -> Int' n
mkInt' i = j
  where
    bs = bitSizeInt' j
    i' = fromIntegral i .&. (bit bs-1)
    j | testBit i' (bs-1) = Int' (i' - bit bs)
      | otherwise         = Int' i'

-- | Constraint synonym providing the context of the underlying native integer type(s)
type Int'Ctx n = (KnownNat n, Bits (IntLeast n), Integral (IntLeast n), Show (IntLeast n), Read (IntLeast n))


deriving instance (Int'Ctx n) => Eq  (Int' n)
deriving instance (Int'Ctx n) => Ord (Int' n)

instance Int'Ctx n => Bounded (Int' n) where
    minBound = let x = Int' (negate (bit (bitSizeInt' x - 1))) in x
    maxBound = let x = Int' (bit (max (bitSizeInt' x - 1) 0) - 1) in x

instance Int'Ctx n => Show (Int' n) where
    showsPrec p (Int' i) = showsPrec p i

instance Int'Ctx n => Real (Int' n) where
    toRational (Int' i) = toRational i

instance Int'Ctx n => Read (Int' n) where
    readsPrec p s = [ (mkInt' (a :: IntLeast n), n) | (a,n) <- readsPrec p s ]

instance NFData (Int' n) where
    rnf (Int' i) = i `seq` ()

instance Int'Ctx n => Enum (Int' n) where
    toEnum i | fromEnum (minBound :: Int' n) <= i
             , i <= fromEnum (maxBound :: Int' n) = Int' (fromIntegral i)
             | otherwise = error "toEnum{Int' n}: outside of bounds"

    fromEnum (Int' j) = fromIntegral j

    enumFrom     = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance Int'Ctx n => Ix (Int' n) where
    range (l,u)                        = [l..u]
    unsafeIndex (Int' l, _) (Int' i)   = fromIntegral (i-l)
    inRange (Int' l, Int' u) (Int' i)  = l <= i && i <= u

instance Int'Ctx n => Bits (Int' n) where
    isSigned _   = True
    bitSize      = bitSizeInt'
    bitSizeMaybe = Just . bitSizeInt'

    bit n = let b = Int' (if 0 <= n && n < bitSizeInt' b then bit n else 0) in b

    (Int' a) .&.   (Int' b) = Int' (a .&. b)
    (Int' a) .|.   (Int' b) = Int' (a .|. b)
    (Int' a) `xor` (Int' b) = Int' (a `xor` b)

    testBit i@(Int' a) n
      | n < bitSizeInt' i  = testBit a n
      | otherwise          = False

    complement (Int' a)   = Int' (complement a)
    popCount   i@(Int' a)
      | a < 0     = popCount (a .&. (bit (bitSizeInt' i) - 1))
      | otherwise = popCount a
    shift      (Int' a) n = mkInt' (shift a n)

    rotateL  w@(Int' a) n
      | n' == 0   = w
      | n   < 0   = error "rotateL: negative argument"
      | otherwise = mkInt' $ (a `shift` n') .|. ((a `shift` (n'-ws)) .&. mask)
      where
        n' = n `rem` ws
        ws = bitSizeInt' w
        mask = bit n' - 1

    rotateR w n
      | n   < 0   = error "rotateL: negative argument"
      | otherwise = rotateL w (ws-(n `rem` ws))
      where
        ws = bitSizeInt' w

instance Int'Ctx n => FiniteBits (Int' n) where
    finiteBitSize = bitSizeInt'

instance Int'Ctx n => Num (Int' n) where
    fromInteger = mkInt'

    (Int' a) + (Int' b) = mkInt' (a+b)
    (Int' a) * (Int' b) = mkInt' (a*b)

    negate (Int' a) = mkInt' (negate a)
    abs    (Int' a) = mkInt' (abs a)

    signum (Int' a) = Int' (signum a)

instance Int'Ctx n => Integral (Int' n) where
    toInteger (Int' a) = toInteger a

    quot (Int' a) (Int' b) = Int' (quot a b)
    rem  (Int' a) (Int' b) = Int' (rem a b)
    div  (Int' a) (Int' b) = Int' (div a b)
    mod  (Int' a) (Int' b) = Int' (mod a b)

    quotRem (Int' a) (Int' b) = let (q,r) = quotRem a b in (Int' q, Int' r)
    divMod  (Int' a) (Int' b) = let (q,r) = divMod a b  in (Int' q, Int' r)
