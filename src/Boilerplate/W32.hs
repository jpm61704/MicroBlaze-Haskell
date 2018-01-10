{-|
Module : Boilerplate.W32
Description : Utility functions for working with 32-bit data
-}
{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate.W32 where

import           Boilerplate
import qualified Boilerplate.Bit as B
import qualified Boilerplate.W8  as W8
import qualified Prelude         as P

-- | zero value (00000000 00000000 00000000 00000000)
zero ∷ W32
zero = W32 (W8.zero) (W8.zero) (W8.zero) (W8.zero)

-- | one value (00000000 00000000 00000000 00000001)
one :: W32
one = W32 (W8.zero) (W8.zero) (W8.zero) (W8.one)

-- * Logic

-- | Logical AND
and ∷ W32 → W32 → W32
and (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) = W32 (W8.and b1 c1) (W8.and b2 c2)
                                              (W8.and b3 c3) (W8.and b4 c4)

-- | Logical OR
or ∷ W32 → W32 → W32
or (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) = W32 (W8.or b1 c1) (W8.or b2 c2)
                                             (W8.or b3 c3) (W8.or b4 c4)

-- | Logical NOT
not ∷ W32 → W32
not (W32 b1 b2 b3 b4) = W32 (W8.not b1) (W8.not b2) (W8.not b3) (W8.not b4)

-- | equality
(==) :: W32 -> W32 -> Bit
(W32 b0 b1 b2 b3) == (W32 c0 c1 c2 c3) = B.and (B.and e0 e1) (B.and e2 e3)
  where e0 = b0 W8.== c0
        e1 = b1 W8.== c1
        e2 = b2 W8.== c2
        e3 = b3 W8.== c3


-- * Conversions(Extensions)

-- | two's complement 16-bit sign extension
signExtendW16 ∷ W16 → W32
signExtendW16 w@(W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) =
  W32 byte0 byte0 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 b8 b9 b10 b11 b12 b13 b14 b15)
  where byte0 = W8 b0 b0 b0 b0 b0 b0 b0 b0

-- | unsigned 16-bit extension
unsignedExtendW16 ∷ W16 → W32
unsignedExtendW16 (W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = W32 W8.zero W8.zero (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 b8 b9 b10 b11 b12 b13 b14 b15)

-- | two's complement byte sign extension
signExtendW8 ∷ W8 → W32
signExtendW8 w@(W8 b0 b1 b2 b3 b4 b5 b6 b7) =
  W32 byte0 byte0 byte0 (W8 b0 b1 b2 b3 b4 b5 b6 b7)
  where byte0 = W8 b0 b0 b0 b0 b0 b0 b0 b0

-- | unsigned byte extension
unsignedExtendW8 ∷ W8 → W32
unsignedExtendW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W32 W8.zero W8.zero W8.zero (W8 b0 b1 b2 b3 b4 b5 b6 b7)

-- | back-extends 16-bit data (useful for certain register operations)
--
-- example : 1010 0011 -> 1010 0011 0000 0000
backExtendW16 ∷ W16 → W32
backExtendW16 (W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) = W32 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 b8 b9 b10 b11 b12 b13 b14 b15) W8.zero W8.zero

-- * Arithmetic

-- | addition with carries
add ∷ W32 → W32 → Bit → (Bit, W32)
add (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) ci = (co, W32 d1 d2 d3 d4)
  where ( co, d1) = W8.add b1 c1 co1
        (co1, d2) = W8.add b2 c2 co2
        (co2, d3) = W8.add b3 c3 co3
        (co3, d4) = W8.add b4 c4 ci

-- | subtraction with carries
subtract ∷ W32 → W32 → Bit → (Bit, W32)
subtract (W32 x0 x1 x2 x3) (W32 y0 y1 y2 y3) ci = (c0, W32 d0 d1 d2 d3)
  where (c0, d0) = W8.subtract x0 y0 c1
        (c1, d1) = W8.subtract x1 y1 c2
        (c2, d2) = W8.subtract x2 y2 c3
        (c3, d3) = W8.subtract x3 y3 ci

-- | subtraction-based comparison with sign-preservation
signedCompare ∷ W32 → W32 → W32
signedCompare x y = cmp
  where (_, sub@(W32 (W8 b0 b1 b2 b3 b4 b5 b6 b7) byte1 byte2 byte3)) = subtract y x C
        msb = case isZero sub of
                S → b0
                C → isPositive sub
        cmp = W32 (W8 msb b1 b2 b3 b4 b5 b6 b7) byte1 byte2 byte3

-- | unsigned subtraction comparison
unsignedCompare ∷ W32 → W32 → W32
unsignedCompare x y = P.snd (subtract x y C)

-- ** Value comparision

-- | test if value is negative under two's complement
isNegative :: W32 -> Bit
isNegative (W32 b0 _ _ _) = W8.isNegative b0

-- | test if a value is positive or zero under two's complement
isPositive ∷ W32 → Bit
isPositive = B.not P.. isNegative

-- | test if a value is zero
isZero ∷ W32 → Bit
isZero = (==) zero

-- | test if a value is greater than zero under two's complement
greaterThanZero ∷ W32 → Bit
greaterThanZero x = B.and (isPositive x) (B.not (isZero x))

-- | test if a value is less than or equal to zero under two's complement
lessThanOrEqualToZero ∷ W32 → Bit
lessThanOrEqualToZero x = B.or (isNegative x) (isZero x)

-- | reverse subtraction
--
-- rb + not(ra) + carry
reverseSubtraction ∷ W32 → W32 → Bit → (Bit, W32)
reverseSubtraction ra rb ci = add rb (not ra) ci
