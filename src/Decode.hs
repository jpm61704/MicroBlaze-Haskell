{-# LANGUAGE UnicodeSyntax #-}
-- |

module Decode where

import           Boilerplate
import           InsSet


decode ∷ W32 → Ins
decode wd = case _opcode raw of
  (W6 C C C C C C) → Add rD rA rB
  (W6 C C C C S C) → Addc rD rA rB
  (W6 C C C S C C) → Addk rD rA rB
  (W6 C C C S S C) → Addkc rD rA rB
  (W6 C C S C C C) → Addi rD rA imm
  (W6 C C S C S C) → Addic rD rA imm
  (W6 C C S S C C) → Addik rD rA imm
  (W6 C C S S S C) → Addikc rD rA imm
  (W6 S C C C C S) → And rD rA rB
  (W6 S C S C C S) → Andi rD rA imm
  (W6 S C C C S S) → Andn rD rA rB
  (W6 S C S C S S) → Andni rD rA imm
  (W6 S C C S S S)
    | delay     → Beqd rA rB zeroW11
    | otherwise → Beq rA rB zeroW11
  (W6 S C S S S S)
    | delay     → Beqid rA imm
    | otherwise → Beqi rA imm
  where raw    = format wd
        rD     = decodeRegister $ _dest raw
        rA     = decodeRegister $ _srcA raw
        rB     = decodeRegister $ regB raw
        ext_op = _dest raw
        delay  = firstBitSet ext_op
        imm    = _srcB raw


data RawIns = RIns { _opcode ∷ W6
                   , _dest   ∷ W5
                   , _srcA   ∷ W5
                   , _srcB   ∷ W16 } deriving Show

regB :: RawIns → W5
regB ins = case _srcB ins of
  (W16 b0 b1 b2 b3 b4 _ _ _ _ _ _ _ _ _ _ _) → W5 b0 b1 b2 b3 b4

format ∷ W32 → RawIns
format = undefined



decodeRegister ∷ W5 → MBReg
decodeRegister reg = case reg of
    (W5 C C C C C) -> R0
    (W5 C C C C S) -> R1
    (W5 C C C S C) -> R2
    (W5 C C C S S) -> R3
    (W5 C C S C C) -> R4
    (W5 C C S C S) -> R5
    (W5 C C S S C) -> R6
    (W5 C C S S S) -> R7
    (W5 C S C C C) -> R8
    (W5 C S C C S) -> R9
    (W5 C S C S C) -> R10
    (W5 C S C S S) -> R11
    (W5 C S S C C) -> R12
    (W5 C S S C S) -> R13
    (W5 C S S S C) -> R14
    (W5 C S S S S) -> R15
    (W5 S C C C C) -> R16
    (W5 S C C C S) -> R17
    (W5 S C C S C) -> R18
    (W5 S C C S S) -> R19
    (W5 S C S C C) -> R20
    (W5 S C S C S) -> R21
    (W5 S C S S C) -> R22
    (W5 S C S S S) -> R23
    (W5 S S C C C) -> R24
    (W5 S S C C S) -> R25
    (W5 S S C S C) -> R26
    (W5 S S C S S) -> R27
    (W5 S S S C C) -> R28
    (W5 S S S C S) -> R29
    (W5 S S S S C) -> R30
    (W5 S S S S S) -> R31


zeroW11 :: W11
zeroW11 = W11 C C C C C C C C C C C

firstBitSet :: W5 → Bool
firstBitSet (W5 S _ _ _ _) = True
firstBitSet _              = False
