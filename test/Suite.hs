{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Bits
import Data.Int
import Data.Word
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck hiding ((.&.))
import Test.HUnit hiding (Test)
import Control.Monad

import Data.Int.Fixed
import Data.Word.Fixed

testProps :: forall t . (Bounded t, FiniteBits t, Bits t, Read t, Show t, Arbitrary t, Integral t) => t -> [Test]
testProps _ =
    (if (finiteBitSize (0::t)) > 1 then
    [ testProperty "Read"    (testRead      :: t-> Bool)
    , testProperty "Bounded" (testBounded   :: Integer -> Bool)

    , testProperty "add"     (testNumAdd    :: t -> t-> Bool)
    , testProperty "sub"     (testNumSub    :: t -> t-> Bool)
    , testProperty "mul"     (testNumMul    :: t -> t-> Bool)
    , testProperty "abs"     (testNumAbs    :: t-> Bool)

    , testProperty "or"      (testBitsOr    :: t -> t-> Bool)
    , testProperty "and"     (testBitsAnd   :: t -> t-> Bool)
    , testProperty "xor"     (testBitsXor   :: t -> t-> Bool)

    , testProperty "complement" (testBitsCompl :: t-> Bool)
       , testGroup "Word' 65" $ testProps (0::Word' 65)
       , testGroup "Int' 66" $ testProps (0::Int' 66)
       , testGroup "Word' 66" $ testProps (0::Word' 66)

       , testGroup "Int' 128" $ testProps (0::Int' 128)
       , testGroup "Word' 128" $ testProps (0::Word' 128)
       ]


instance Int'Ctx n => Arbitrary (Int' n) where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink    = shrinkIntegral

instance Word'Ctx n => Arbitrary (Word' n) where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink    = shrinkIntegral
