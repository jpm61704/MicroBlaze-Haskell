{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate where

import           Control.Monad.Identity hiding (when)
import           Control.Monad.State    hiding (when)


-- begin primitive boilerplate
data Bit = C | S deriving (Eq,Enum)

instance Show Bit where
  show C = "0"
  show S = "1"

notBit S = C
notBit C = S
eqBit S  S   = S
eqBit C C    = S
eqBit _    _ = C
andBit S b = b
andBit C _ = C
orBit C b = b
orBit S _ = S
xorBit S b = notBit b
xorBit C b = b
plusCBit C C C    = (C,C)
plusCBit C C  S   = (C, S)
plusCBit C  S C   = (C, S)
plusCBit C  S  S  = ( S,C)
plusCBit  S C C   = (C, S)
plusCBit  S C  S  = ( S,C)
plusCBit  S  S C  = ( S,C)
plusCBit  S  S  S = ( S, S)
minusCBit  C C C    = (C,C)
minusCBit  C C  S   = ( S, S)
minusCBit  C  S C   = ( S, S)
minusCBit  C  S  S  = ( S,C)
minusCBit   S C C   = (C, S)
minusCBit   S C  S  = (C,C)
minusCBit   S  S C  = (C,C)
minusCBit   S  S  S = ( S,C)

data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
zeroW8 = W8 C C C C C C C C
oneW8  = W8 C C C C C C C  S
notW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 (notBit b0) (notBit b1) (notBit b2) (notBit b3) (notBit b4) (notBit b5) (notBit b6) (notBit b7)
andW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (andBit b0 c0) (andBit b1 c1) (andBit b2 c2) (andBit b3 c3) (andBit b4 c4) (andBit b5 c5) (andBit b6 c6) (andBit b7 c7)


xorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = W8 (xorBit b0 c0) (xorBit b1 c1) (xorBit b2 c2) (xorBit b3 c3) (xorBit b4 c4) (xorBit b5 c5) (xorBit b6 c6) (xorBit b7 c7)
eqW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) = andBit (andBit (andBit (eqBit b0 c0) (eqBit b1 c1)) (andBit (eqBit b2 c2) (eqBit b3 c3))) (andBit (andBit (eqBit b4 c4) (eqBit b5 c5)) (andBit (eqBit b6 c6) (eqBit b7 c7)))
rolW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b1 b2 b3 b4 b5 b6 b7 b0
rorW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) = W8 b7 b0 b1 b2 b3 b4 b5 b6

plusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = plusCBit b0 c0 co1
       (co1,d1) = plusCBit b1 c1 co2
       (co2,d2) = plusCBit b2 c2 co3
       (co3,d3) = plusCBit b3 c3 co4
       (co4,d4) = plusCBit b4 c4 co5
       (co5,d5) = plusCBit b5 c5 co6
       (co6,d6) = plusCBit b6 c6 co7
       (co7,d7) = plusCBit b7 c7 ci

data W32  = W32 Bit Bit Bit Bit Bit Bit Bit Bit
                Bit Bit Bit Bit Bit Bit Bit Bit
                Bit Bit Bit Bit Bit Bit Bit Bit
                Bit Bit Bit Bit Bit Bit Bit Bit
              deriving (Eq,Show)

w32_0 = W32 C C C C C C C C C C C C C
            C C C C C C C C C C C C C
            C C C C C C

w32_4 = W32 C C C C C C C C C C C C C
            C C C C C C C C C C C C C
            C C C S C C

w32_8 = W32 C C C C C C C C C C C C C
            C C C C C C C C C C C C C
            C C S C C C

--
-- 2's complement or unsigned arithmetic. I haven't thought about this bullshit since
-- 1989. Does it make a difference with addition here? It doesn't seem too.
-- But it does bear making sure that I'm not building nonsense.
--
plusW32  (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
         (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15
              c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31)
         ci
         = (W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15
                d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31)
 where
       (co0,d0)   = plusCBit b0  c0  co1
       (co1,d1)   = plusCBit b1  c1  co2
       (co2,d2)   = plusCBit b2  c2  co3
       (co3,d3)   = plusCBit b3  c3  co4
       (co4,d4)   = plusCBit b4  c4  co5
       (co5,d5)   = plusCBit b5  c5  co6
       (co6,d6)   = plusCBit b6  c6  co7
       (co7,d7)   = plusCBit b7  c7  co8
       (co8,d8)   = plusCBit b8  c8  co9
       (co9,d9)   = plusCBit b9  c9  co10
       (co10,d10) = plusCBit b10 c10 co11
       (co11,d11) = plusCBit b11 c11 co12
       (co12,d12) = plusCBit b12 c12 co13
       (co13,d13) = plusCBit b13 c13 co14
       (co14,d14) = plusCBit b14 c14 co15
       (co15,d15) = plusCBit b15 c15 co16
       (co16,d16) = plusCBit b16 c16 co17
       (co17,d17) = plusCBit b17 c17 co18
       (co18,d18) = plusCBit b18 c18 co19
       (co19,d19) = plusCBit b19 c19 co20
       (co20,d20) = plusCBit b20 c20 co21
       (co21,d21) = plusCBit b21 c21 co22
       (co22,d22) = plusCBit b22 c22 co23
       (co23,d23) = plusCBit b23 c23 co24
       (co24,d24) = plusCBit b24 c24 co25
       (co25,d25) = plusCBit b25 c25 co26
       (co26,d26) = plusCBit b26 c26 co27
       (co27,d27) = plusCBit b27 c27 co28
       (co28,d28) = plusCBit b28 c28 co29
       (co29,d29) = plusCBit b29 c29 co30
       (co30,d30) = plusCBit b30 c30 co31
       (co31,d31) = plusCBit b31 c31 ci

plusCW32 (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15
              b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
         (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15
              c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31)
         ci
         = (co0, W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15
                     d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31)
 where
       (co0,d0)   = plusCBit b0  c0  co1
       (co1,d1)   = plusCBit b1  c1  co2
       (co2,d2)   = plusCBit b2  c2  co3
       (co3,d3)   = plusCBit b3  c3  co4
       (co4,d4)   = plusCBit b4  c4  co5
       (co5,d5)   = plusCBit b5  c5  co6
       (co6,d6)   = plusCBit b6  c6  co7
       (co7,d7)   = plusCBit b7  c7  co8
       (co8,d8)   = plusCBit b8  c8  co9
       (co9,d9)   = plusCBit b9  c9  co10
       (co10,d10) = plusCBit b10 c10 co11
       (co11,d11) = plusCBit b11 c11 co12
       (co12,d12) = plusCBit b12 c12 co13
       (co13,d13) = plusCBit b13 c13 co14
       (co14,d14) = plusCBit b14 c14 co15
       (co15,d15) = plusCBit b15 c15 co16
       (co16,d16) = plusCBit b16 c16 co17
       (co17,d17) = plusCBit b17 c17 co18
       (co18,d18) = plusCBit b18 c18 co19
       (co19,d19) = plusCBit b19 c19 co20
       (co20,d20) = plusCBit b20 c20 co21
       (co21,d21) = plusCBit b21 c21 co22
       (co22,d22) = plusCBit b22 c22 co23
       (co23,d23) = plusCBit b23 c23 co24
       (co24,d24) = plusCBit b24 c24 co25
       (co25,d25) = plusCBit b25 c25 co26
       (co26,d26) = plusCBit b26 c26 co27
       (co27,d27) = plusCBit b27 c27 co28
       (co28,d28) = plusCBit b28 c28 co29
       (co29,d29) = plusCBit b29 c29 co30
       (co30,d30) = plusCBit b30 c30 co31
       (co31,d31) = plusCBit b31 c31 ci

andW32 (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15
           b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
       (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15
           c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31)
         = (W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15
                d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31)
      where d0  = andBit b0 c0
            d1  = andBit b1 c1
            d2  = andBit b2 c2
            d3  = andBit b3 c3
            d4  = andBit b4 c4
            d5  = andBit b5 c5
            d6  = andBit b6 c6
            d7  = andBit b7 c7
            d8  = andBit b8 c8
            d9  = andBit b9 c9
            d10 = andBit b10 c10
            d11 = andBit b11 c11
            d12 = andBit b12 c12
            d13 = andBit b13 c13
            d14 = andBit b14 c14
            d15 = andBit b15 c15
            d16 = andBit b16 c16
            d17 = andBit b17 c17
            d18 = andBit b18 c18
            d19 = andBit b19 c19
            d20 = andBit b20 c20
            d21 = andBit b21 c21
            d22 = andBit b22 c22
            d23 = andBit b23 c23
            d24 = andBit b24 c24
            d25 = andBit b25 c25
            d26 = andBit b26 c26
            d27 = andBit b27 c27
            d28 = andBit b28 c28
            d29 = andBit b29 c29
            d30 = andBit b30 c30
            d31 = andBit b31 c31

orW32 (W32  b0  b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15
           b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
      (W32  c0  c1  c2  c3  c4  c5  c6  c7  c8  c9 c10 c11 c12 c13 c14 c15
           c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31)
         = (W32  d0  d1  d2  d3  d4  d5  d6  d7  d8  d9 d10 d11 d12 d13 d14 d15
                d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31)
      where d0  = orBit b0  c0
            d1  = orBit b1  c1
            d2  = orBit b2  c2
            d3  = orBit b3  c3
            d4  = orBit b4  c4
            d5  = orBit b5  c5
            d6  = orBit b6  c6
            d7  = orBit b7  c7
            d8  = orBit b8  c8
            d9  = orBit b9  c9
            d10 = orBit b10 c10
            d11 = orBit b11 c11
            d12 = orBit b12 c12
            d13 = orBit b13 c13
            d14 = orBit b14 c14
            d15 = orBit b15 c15
            d16 = orBit b16 c16
            d17 = orBit b17 c17
            d18 = orBit b18 c18
            d19 = orBit b19 c19
            d20 = orBit b20 c20
            d21 = orBit b21 c21
            d22 = orBit b22 c22
            d23 = orBit b23 c23
            d24 = orBit b24 c24
            d25 = orBit b25 c25
            d26 = orBit b26 c26
            d27 = orBit b27 c27
            d28 = orBit b28 c28
            d29 = orBit b29 c29
            d30 = orBit b30 c30
            d31 = orBit b31 c31


plusW8 a b = plusCW8 a b C
negW8 w = snd $ plusW8 (notW8 w) oneW8
minusCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8 c0 c1 c2 c3 c4 c5 c6 c7) ci = (co0,W8 d0 d1 d2 d3 d4 d5 d6 d7)
 where (co0,d0) = minusCBit b0 c0 co1
       (co1,d1) = minusCBit b1 c1 co2
       (co2,d2) = minusCBit b2 c2 co3
       (co3,d3) = minusCBit b3 c3 co4
       (co4,d4) = minusCBit b4 c4 co5
       (co5,d5) = minusCBit b5 c5 co6
       (co6,d6) = minusCBit b6 c6 co7
       (co7,d7) = minusCBit b7 c7 ci
shlCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b0,W8 b1 b2 b3 b4 b5 b6 b7 ci)
shrCW8 (W8 b0 b1 b2 b3 b4 b5 b6 b7) ci = (b7,W8 ci b0 b1 b2 b3 b4 b5 b6)
msbW8 (W8 b _ _ _ _ _ _ _) = b
lsbW8 (W8 _ _ _ _ _ _ _ b) = b

-- end primitive boilerplate

-- Kleisli composition
(<>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f <> g = \ a -> f a >>= g



--
-- new boilerplate for DLX.
--

top6 (W32 b0 b1 b2 b3 b4 b5 _ _ _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _ _         _ _ _ _ _ _ _ _) = W6 b0 b1 b2 b3 b4 b5


byte0, byte1, byte2, byte3 :: W32 -> W8
byte0 (W32 b0 b1 b2 b3 b4 b5 b6 b7
           _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _) = W8 b0 b1 b2 b3 b4 b5 b6 b7

byte1 (W32 _ _ _ _ _ _ _ _
           b0 b1 b2 b3 b4 b5 b6 b7
           _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _) = W8 b0 b1 b2 b3 b4 b5 b6 b7

byte2 (W32 _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _
           b0 b1 b2 b3 b4 b5 b6 b7
           _ _ _ _ _ _ _ _) = W8 b0 b1 b2 b3 b4 b5 b6 b7

byte3 (W32 _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _
           _ _ _ _ _ _ _ _
           b0 b1 b2 b3 b4 b5 b6 b7) = W8 b0 b1 b2 b3 b4 b5 b6 b7

data W5  =  W5 Bit Bit Bit Bit Bit deriving Eq
instance Show W5 where
  show (W5 b0 b1 b2 b3 b4) = concat $ map show [b0,b1,b2,b3,b4]
{-
instance Enum W5 where
  succ (W5 C C C C C) = W5 C C C C S
  succ (W5 C C C C S) = W5 C C C S C
  succ (W5 C C C S C) = W5 C C C S S
  succ (W5 C C C S S) = W5 C C S C C
  succ (W5 C C S C C) = W5 C C S C S
  succ (W5 C C S C S) = W5 C C S S C
  succ (W5 C C S S C) = W5 C C S S S
  succ (W5 C C S S S) = W5 C S C C C

  succ (W5 C S C C C) = W5 C S C C S
  succ (W5 C S C C S) = W5 C S C S C
  succ (W5 C S C S C) = W5 C S C S S
  succ (W5 C S C S S) = W5 C S S C C
  succ (W5 C S S C C) = W5 C S S C S
  succ (W5 C S S C S) = W5 C S S S C
  succ (W5 C S S S C) = W5 C S S S S
  succ (W5 C S S S S) = W5 S C C C C

  succ (W5 S C C C C) = W5 S C C C S
  succ (W5 S C C C S) = W5 S C C S C
  succ (W5 S C C S C) = W5 S C C S S
  succ (W5 S C C S S) = W5 S C S C C
  succ (W5 S C S C C) = W5 S C S C S
  succ (W5 S C S C S) = W5 S C S S C
  succ (W5 S C S S C) = W5 S C S S S
  succ (W5 S C S S S) = W5 S S C C C

  succ (W5 S S C C C) = W5 S S C C S
  succ (W5 S S C C S) = W5 S S C S C
  succ (W5 S S C S C) = W5 S S C S S
  succ (W5 S S C S S) = W5 S S S C C
  succ (W5 S S S C C) = W5 S S S C S
  succ (W5 S S S C S) = W5 S S S S C
  succ (W5 S S S S C) = W5 S S S S S
  succ (W5 S S S S S) = undefined
-}

data W6  =  W6 Bit Bit Bit Bit Bit Bit
data W11 = W11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
data W16 = W16 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving Show
data W26 = W26 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit

zero16 = W16 C C C C C C C C C C C C C C C C

zero32 = W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C

one32 = W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S

-- concatenation
(||) :: W16 -> W16 -> W32
(W16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) || (W16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0) = W32 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0

--
-- Some phoney-baloney operations.
--

w32_lte :: W32 -> W32 -> Bool
w32_lte = error "unimplemented"

w32_lt :: W32 -> W32 -> Bool
w32_lt = error "unimplemented"

w32_ne :: W32 -> W32 -> Bool
w32_ne = error "unimplemented"

signExtendW16 ∷ W16 → W32
signExtendW16 w@(W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) =
  W32 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15

notW32 ∷ W32 → W32
notW32 (W32 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31) =
  (W32 (notBit b0) (notBit b1) (notBit b2) (notBit b3) (notBit b4) (notBit b5) (notBit b6) (notBit b7)
       (notBit b8) (notBit b9) (notBit b10) (notBit b11) (notBit b12) (notBit b13) (notBit b14) (notBit b15)
       (notBit b16) (notBit b17) (notBit b18) (notBit b19) (notBit b20) (notBit b21) (notBit b22) (notBit b23)
       (notBit b24) (notBit b25) (notBit b26) (notBit b27) (notBit b28) (notBit b29) (notBit b30) (notBit b31))
