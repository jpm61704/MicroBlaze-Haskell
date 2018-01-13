{-|
Module : Boilerplate.W8
Description : Utility functions for working with Byte-sized data
-}
{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate.W8 where

import           Boilerplate
import qualified Boilerplate.Bit as B
import qualified Prelude         as P

-- | zero-byte (00000000)
zero ∷ W8
zero = W8 C C C C C C C C

-- | one-byte (00000001)
one :: W8
one = W8 C C C C C C C S

-- * Logic

-- | logical not
not ∷ W8 → W8
not (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 (B.not b0) (B.not b1) (B.not b2) (B.not b3)
                                      (B.not b4) (B.not b5) (B.not b6) (B.not b7)

-- | logical and
and :: W8 → W8 → W8
and (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (B.and b0 c0) (B.and b1 c1)
                                                                   (B.and b2 c2) (B.and b3 c3)
                                                                   (B.and b4 c4) (B.and b5 c5)
                                                                   (B.and b6 c6) (B.and b7 c7)

-- | logical or
or :: W8 → W8 → W8
or (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (B.or b0 c0) (B.or b1 c1)
                                                                   (B.or b2 c2) (B.or b3 c3)
                                                                   (B.or b4 c4) (B.or b5 c5)
                                                                   (B.or b6 c6) (B.or b7 c7)

-- | logical exclusive or
xor ∷ W8 → W8 → W8
xor (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (B.xor b0 c0) (B.xor b1 c1)
                                                                   (B.xor b2 c2) (B.xor b3 c3)
                                                                   (B.xor b4 c4) (B.xor b5 c5)
                                                                   (B.xor b6 c6) (B.xor b7 c7)

-- | logical equality (as opposed to bitwise)
(==) ∷ W8 → W8 → Bit
(==) (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = B.and (B.and (B.and (b0 B.== c0) (b1 B.== c1)) (B.and (b2 B.== c2) (b3 B.== c3))) (B.and (B.and (b4 B.== c4) (b5 B.== c5)) (B.and (b6 B.== c6) (b7 B.== c7)))


-- * Bitwise operations

-- | left rotation
rotateLeft ∷ W8 → W8
rotateLeft (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b1 b2 b3 b4 b5 b6 b7 b0

-- | right rotation
rotateRight ∷ W8 → W8
rotateRight (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b7 b0 b1 b2 b3 b4 b5 b6

-- | left shift with carry
shiftLeft ∷ W8 → Bit → (Bit, W8)
shiftLeft (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b0,W8 b1 b2 b3 b4 b5 b6 b7 ci)

-- | right shift with carry
shiftRight ∷ W8 → Bit → (Bit, W8)
shiftRight (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7,W8 ci b0 b1 b2 b3 b4 b5 b6)

-- | most significant bit of byte
--
-- also is the sign-bit
mostSignificantBit ∷ W8 → Bit
mostSignificantBit (W8 b _ _ _ _ _ _ _) = b

-- | least significant bit of byte
leastSignificantBit ∷ W8 → Bit
leastSignificantBit (W8 _ _ _ _ _ _ _ b) = b


-- * Arithmetic

-- | addition with carry
add ∷ W8 → W8 → Bit → (Bit, W8)
add (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = B.plus b0 c0 co1
       (co1,d1) = B.plus b1 c1 co2
       (co2,d2) = B.plus b2 c2 co3
       (co3,d3) = B.plus b3 c3 co4
       (co4,d4) = B.plus b4 c4 co5
       (co5,d5) = B.plus b5 c5 co6
       (co6,d6) = B.plus b6 c6 co7
       (co7,d7) = B.plus b7 c7 ci

-- | subtraction with carry
subtract ∷ W8 → W8 → Bit → (Bit, W8)
subtract (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = B.minus b0 c0 co1
       (co1,d1) = B.minus b1 c1 co2
       (co2,d2) = B.minus b2 c2 co3
       (co3,d3) = B.minus b3 c3 co4
       (co4,d4) = B.minus b4 c4 co5
       (co5,d5) = B.minus b5 c5 co6
       (co6,d6) = B.minus b6 c6 co7
       (co7,d7) = B.minus b7 c7 ci

-- | two's complement negation
negative ∷ W8 → W8
negative w = P.snd (add (not w) one C)

-- | tests negativity
isNegative :: W8 -> Bit
isNegative w = (mostSignificantBit w) B.== S

-- * Bit Shifts

arithmeticShiftRight ∷ W8 → Bit → (Bit, W8)
arithmeticShiftRight (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7, W8 ci b0 b1 b2 b3 b4 b5 b6)

logicalShiftRight ∷ W8 → (Bit, W8)
logicalShiftRight (W8 b0 b1 b2 b3 b4 b5 b6 b7) = (b7, W8 C b0 b1 b2 b3 b4 b5 b6)
