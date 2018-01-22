{-|
Module : Boilerplate.Bit

-}
{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate.Bit where

import           Boilerplate

import qualified Prelude     as P

-- * Logic

-- | bit negation
-- @
-- not S = C
-- not C = S
-- @
not ∷ Bit → Bit
not S = C
not C = S

-- | equality
(==) ∷ Bit → Bit → Bit
(==) S S = S
(==) C C = S
(==) _ _ = C

-- | logical AND
and ∷ Bit → Bit → Bit
and S b = b
and C _ = C

-- | logical OR
or :: Bit → Bit → Bit
or C b = b
or S _ = S

-- | logical exclusive OR
xor ∷ Bit → Bit → Bit
xor S b = not b
xor C b = b

-- * Arithmetic

-- | Binary additon with carries
plus ∷ Bit          -- ^ first bit to add
     → Bit          -- ^ second bit to add
     → Bit          -- ^ carry bit input
     → (Bit, Bit)   -- ^ (Carry Bit, Sum Bit)
plus C C C = (C,C)
plus C C S = (C,S)
plus C S C = (C,S)
plus C S S = (S,C)
plus S C C = (C,S)
plus S C S = (S,C)
plus S S C = (S,C)
plus S S S = (S,S)

-- | Binary subtraction with carries
minus ∷ Bit          -- ^ minuend
      → Bit          -- ^ subtraend
      → Bit          -- ^ carry bit input
      → (Bit, Bit)   -- ^ (carry bit, difference bit)
minus C C C = (C,C)
minus C C S = (S,S)
minus C S C = (S,S)
minus C S S = (S,C)
minus S C C = (C,S)
minus S C S = (C,C)
minus S S C = (C,C)
minus S S S = (S,C)
