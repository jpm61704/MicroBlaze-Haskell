{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate.W32 where

import           Boilerplate
import qualified Boilerplate.Bit as B
import qualified Boilerplate.W8  as W8
import qualified Prelude         as P


zero ∷ W32
zero = W32 (W8.zero) (W8.zero) (W8.zero) (W8.zero)

add ∷ W32 → W32 → Bit → (Bit, W32)
add (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) ci = (co, W32 d1 d2 d3 d4)
  where ( co, d1) = W8.add b1 c1 co1
        (co1, d2) = W8.add b2 c2 co2
        (co2, d3) = W8.add b3 c3 co3
        (co3, d4) = W8.add b4 c4 ci

and ∷ W32 → W32 → W32
and (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) = W32 (W8.and b1 c1) (W8.and b2 c2)
                                              (W8.and b3 c3) (W8.and b4 c4)

or ∷ W32 → W32 → W32
or (W32 b1 b2 b3 b4) (W32 c1 c2 c3 c4) = W32 (W8.or b1 c1) (W8.or b2 c2)
                                             (W8.or b3 c3) (W8.or b4 c4)

not ∷ W32 → W32
not (W32 b1 b2 b3 b4) = W32 (W8.not b1) (W8.not b2) (W8.not b3) (W8.not b4)

signExtendW16 ∷ W16 → W32
signExtendW16 w@(W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) =
  W32 byte0 byte0 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 b8 b9 b10 b11 b12 b13 b14 b15)
  where byte0 = W8 b0 b0 b0 b0 b0 b0 b0 b0

(==) :: W32 -> W32 -> Bit
(W32 b0 b1 b2 b3) == (W32 c0 c1 c2 c3) = B.and (B.and e0 e1) (B.and e2 e3)
  where e0 = b0 W8.== c0
        e1 = b1 W8.== c1
        e2 = b2 W8.== c2
        e3 = b3 W8.== c3

isNegative :: W32 -> Bit
isNegative (W32 b0 _ _ _) = W8.isNegative b0

isPositive ∷ W32 → Bit
isPositive = B.not P.. isNegative

isZero ∷ W32 → Bit
isZero = (==) zero

greaterThanZero ∷ W32 → Bit
greaterThanZero x = B.and (isPositive x) (B.not (isZero x))

lessThanOrEqualToZero ∷ W32 → Bit
lessThanOrEqualToZero x = B.or (isNegative x) (isZero x)

