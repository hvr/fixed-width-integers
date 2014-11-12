{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Word.Fixed
    ( -- * Generalized fixed-width
      Word'

      -- * Haskell 2010 fixed-width types (re-exported from "Data.Word")
    , Word8, Word16, Word32, Word64

      -- * Internals
    , Word'Ctx

    ) where

import Control.DeepSeq
import Data.Bits
import Data.Ix
import Data.Type.Bool
import Data.Typeable
import Data.Word
import GHC.Arr (Ix(unsafeIndex))
import GHC.Enum
import GHC.TypeLits

----------------------------------------------------------------------------
-- type-indexed fixed-width signed/unsigned integers

-- | Map to an unsigned integer type of at least the requested bit-size
type WordLeast n = If (n <=? 8)  Word8  (
                   If (n <=? 16) Word16 (
                   If (n <=? 32) Word32 (
                   If (n <=? 64) Word64 (
                   Integer)))) -- we could use nats:Natural here but it's not needed

-- | Generalized 'Word' indexed by bit-width @/n/@ with range [0, 2ⁿ-1].
--
-- Values of the type @'Word'' /n/@ implement the same semantics as
-- the fixed-width unsigned integer from "Data.Word" but for general
-- bit-widths.
--
-- @
-- {-# LANGUAGE DataKinds #-}
-- type Word4   = 'Word'' 4
-- type Word7   = 'Word'' 7
-- type Word63  = 'Word'' 63
-- type Word72  = 'Word'' 72
-- type Word256 = 'Word'' 256
-- @
--
-- The current implementation uses the smallest standard fixed-width
-- integer type among 'Word8', 'Word16', 'Word32', and 'Word64' which
-- is capable of holding the requested bit-width. For bit-widths over
-- 64bit, 'Integer' is used as internal storage.
--
-- For the sake of performance and interoperability, it's recommended
-- to avoid using @Word' 8@, @Word' 16@, @Word' 32@, and @Word' 64@ if
-- the respective types from "Data.Word" can be used instead.
--
newtype Word' n = Word' (WordLeast n)
                deriving Typeable

bitSizeWord' :: KnownNat n => Word' n -> Int
bitSizeWord' = fromIntegral . natVal

mkWord' :: (Integral a, Word'Ctx n) => a -> Word' n
mkWord' i = w
  where
    w = Word' (fromIntegral i .&. mask)
    mask = bit (bitSizeWord' w) - 1
{-# INLINE mkWord' #-}

-- | Constraint synonym providing the context of the underlying native integer type(s)
type Word'Ctx n = (KnownNat n, Bits (WordLeast n), Integral (WordLeast n), Show (WordLeast n), Read (WordLeast n))

deriving instance (Word'Ctx n) => Eq  (Word' n)
deriving instance (Word'Ctx n) => Ord (Word' n)

instance Word'Ctx n => Bounded (Word' n) where
    minBound = Word' 0
    maxBound = let x = Word' (bit (bitSizeWord' x) - 1) in x

instance Word'Ctx n => Show (Word' n) where
    showsPrec p (Word' i) = showsPrec p i

instance Word'Ctx n => Real (Word' n) where
    toRational (Word' i) = toRational i

instance Word'Ctx n => Read (Word' n) where
    readsPrec p s = [ (mkWord' (a :: WordLeast n), n) | (a,n) <- readsPrec p s ]

instance NFData (Word' n) where
    rnf (Word' i) = i `seq` ()

instance Word'Ctx n => Enum (Word' n) where
    toEnum i | 0 <= i, i <= fromEnum (maxBound :: Word' n) = Word' (fromIntegral i)
             | otherwise = error "toEnum{Word' n}: outside of bounds"

    fromEnum (Word' j) = fromIntegral j

    enumFrom     = boundedEnumFrom
    enumFromThen = boundedEnumFromThen

instance Word'Ctx n => Ix (Word' n) where
    range (l,u)                          = [l..u]
    unsafeIndex (Word' l, _) (Word' i)   = fromIntegral (i-l)
    inRange (Word' l, Word' u) (Word' i) = l <= i && i <= u

instance Word'Ctx n => Bits (Word' n) where
    isSigned _   = False
    bitSize      = bitSizeWord'
    bitSizeMaybe = Just . bitSizeWord'

    bit n = let b = Word' (if 0 <= n && n < bitSizeWord' b then bit n else 0) in b

    (Word' a) .&.   (Word' b) = Word' (a .&. b)
    (Word' a) .|.   (Word' b) = Word' (a .|. b)
    (Word' a) `xor` (Word' b) = Word' (a `xor` b)

    testBit    (Word' a) n = testBit a n
    complement (Word' a)   = mkWord' (complement a)
    popCount   (Word' a)   = popCount a
    shift      (Word' a) n = mkWord' (shift a n)

    rotate   w@(Word' a) n
      | n' == 0   = w
      | n  > 0    = mkWord' $ (a `shift` n') .|. (a `shift` (n'-ws))
      | otherwise = mkWord' $ (a `shift` n') .|. (a `shift` (n'+ws))
      where
        n' = n `rem` ws
        ws = bitSizeWord' w

instance Word'Ctx n => FiniteBits (Word' n) where
    finiteBitSize = bitSizeWord'

instance Word'Ctx n => Num (Word' n) where
    fromInteger = mkWord'

    (Word' a) + (Word' b) = mkWord' (a+b)
    (Word' a) * (Word' b) = mkWord' (a*b)

    negate (Word' a) = mkWord' (negate a)
    abs    (Word' a) = mkWord' (abs a)

    signum (Word' a) = Word' (signum a)

instance Word'Ctx n => Integral (Word' n) where
    toInteger (Word' a) = toInteger a

    quot (Word' a) (Word' b) = Word' (quot a b)
    rem  (Word' a) (Word' b) = Word' (rem a b)
    div  (Word' a) (Word' b) = Word' (div a b)
    mod  (Word' a) (Word' b) = Word' (mod a b)

    quotRem (Word' a) (Word' b) = let (q,r) = quotRem a b in (Word' q, Word' r)
    divMod  (Word' a) (Word' b) = let (q,r) = divMod a b  in (Word' q, Word' r)
