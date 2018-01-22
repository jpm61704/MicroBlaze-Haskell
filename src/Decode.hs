{-# LANGUAGE UnicodeSyntax #-}
-- |

module Decode where

import           Boilerplate
import           InsSet


data OpType = TypeA (MBReg → MBReg → MBReg → Ins)
            | TypeB (MBReg → MBReg → W16 → Ins)
            | BranchA (MBReg → MBReg → W11 → Ins)
            | BranchB (MBReg → W16 → Ins)
            | UnconditionalBranchA (MBReg → Ins)
            | UnconditionalBranchLinkA (MBReg → MBReg → Ins)
            | UnconditionalBranchB (W16 → Ins)
            | UnconditionalBranchLinkB (MBReg → W16 → Ins)
            | Immed (W16 → Ins)
            | MFS (MBReg → MBSReg → Ins)
            | MTS (MBSReg → MBReg → Ins)



decode' ∷ W6 → W5 → W5 → W16  → OpType
decode' op extD extA extB = case op of
  (W6 C C C C C C) → TypeA Add
  (W6 C C C C S C) → TypeA Addc
  (W6 C C C S C C) → TypeA Addk
  (W6 C C C S S C) → TypeA Addkc
  (W6 C C S C C C) → TypeB Addi
  (W6 C C S C S C) → TypeB Addic
  (W6 C C S S C C) → TypeB Addik
  (W6 C C S S S C) → TypeB Addikc
  (W6 S C C C C S) → TypeA And
  (W6 S C S C C S) → TypeB Andi
  (W6 S C C C S S) → TypeA Andn
  (W6 S C S C S S) → TypeB Andni
  (W6 S C C S S S) → BranchA $ case extD of
                       (W5 S C C C C) → Beqd
                       (W5 C C C C C) → Beq
                       (W5 C C S C S) → Bge
                       (W5 S C S C S) → Bged
                       (W5 C C S C C) → Bgt
                       (W5 S C S C C) → Bgtd
                       (W5 C C C S S) → Ble
                       (W5 S C C S S) → Bled
                       (W5 C C C S C) → Blt
                       (W5 S C C S C) → Bltd
                       (W5 C C C C S) → Bne
                       (W5 S C C C S) → Bned
-- continue on unconditional branch
  (W6 S C S S S S) → BranchB $ case extD of
                       (W5 S C C C C) → Beqid
                       (W5 C C C C C) → Beqi
                       (W5 C C S C S) → Bgei
                       (W5 S C S C S) → Bgeid
                       (W5 C C S C C) → Bgti
                       (W5 S C S C C) → Bgtid
                       (W5 C C C S S) → Blei
                       (W5 S C C S S) → Bleid
                       (W5 C C C S C) → Blti
                       (W5 S C C S C) → Bltid
                       (W5 C C C C S) → Bnei
                       (W5 S C C C S) → Bneid
  (W6 S C C S S C) → case extA of
                       (W5 C C C C C) → UnconditionalBranchA Br
                       (W5 C S C C C) → UnconditionalBranchA Bra
                       (W5 S C C C C) → UnconditionalBranchA Brd
                       (W5 S S C C C) → UnconditionalBranchA Brad
                       (W5 S C S C C) → UnconditionalBranchLinkA Brld
                       (W5 S S S C C) → UnconditionalBranchLinkA Brald
                       (W5 C S S C C) → UnconditionalBranchLinkA Brk
  (W6 S C S S S C) → case extA of
                       (W5 C C C C C) → UnconditionalBranchB Bri
                       (W5 C S C C C) → UnconditionalBranchB Brai
                       (W5 S C C C C) → UnconditionalBranchB Brid
                       (W5 S S C C C) → UnconditionalBranchB Braid
                       (W5 S C S C C) → UnconditionalBranchLinkB Brlid
                       (W5 S S S C C) → UnconditionalBranchLinkB Bralid
                       (W5 C S S C C) → UnconditionalBranchLinkB Brki
  (W6 C S C C C S) → TypeA $ case back11 extB of
                               (W11 C C _ _ _ _ _ _ _ _ _) → Bsrl
                               (W11 C S _ _ _ _ _ _ _ _ _) → Bsra
                               (W11 S C _ _ _ _ _ _ _ _ _) → Bsll
  (W6 C S S C C S) → error "barrel shift immediate not supported"
  (W6 C C C S C S) → TypeA $ case back11 extB of
                               (W11 C C C C C C C C C C C) → Rsubk
                               (W11 _ _ _ _ _ _ _ _ _ C _) → Cmp
                               (W11 _ _ _ _ _ _ _ _ _ S _) → Cmpu
  (W6 C S S C S S) → error "FSL intergace not supported"
  (W6 C S C C S C) → error "Hardware division not yet supported"
  (W6 S C S S C C) → Immed Imm
  (W6 S S C C C C) → TypeA Lbu
  (W6 S S S C C C) → TypeB Lbui
  (W6 S S C C C S) → TypeA Lhu
  (W6 S S S C C S) → TypeB Lhui
  (W6 S S C C S C) → TypeA Lw
  (W6 S S S C S C) → TypeB Lwi
  (W6 S C C S C S) → case (extD, extA) of
                       (W5 C C C C C, _) → MTS Mts
                       (_, W5 C C C C C) → MFS Mfs
  (W6 C S C C C C) → TypeA Mul
  (W6 C S S C C C) → TypeB Muli
  (W6 S C C C C C) → TypeA Or
  (W6 S C S C C C) → TypeB Ori
  (W6 C C C C C S) → TypeA Rsub
  (W6 C C C C S S) → TypeA Rsubc
  (W6 C C C S S S) → TypeA Rsubkc
  -- continue on rsubi




decode ∷ W32 → Ins
decode wd = case (decode' op extD extA imm) of
  TypeA ins   → ins rD rA rB
  TypeB ins   → ins rD rA imm
  BranchA ins → ins rA rB zeroW11
  BranchB ins → ins rA imm
  where raw    = format wd
        rD     = decodeRegister $ _dest raw
        rA     = decodeRegister $ _srcA raw
        rB     = decodeRegister $ regB raw
        op     = _opcode raw
        extD   = _dest raw
        extA   = _srcA raw
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


back11 ∷ W16 → W11
back11 (W16 _ _ _ _ _ b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) =  W11 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10

zeroW11 :: W11
zeroW11 = W11 C C C C C C C C C C C

firstBitSet :: W5 → Bool
firstBitSet (W5 S _ _ _ _) = True
firstBitSet _              = False

secondBitSet ∷ W5 → Bool
secondBitSet (W5 _ S _ _ _) = True
secondBitSet _              = False

thirdBitSet  ∷ W5 → Bool
thirdBitSet (W5 _ _ S _ _) = True
thirdBitSet _              = False


fourthBitSet ∷ W5 → Bool
fourthBitSet (W5 _ _ _ S _) = True
fourthBitSet _              = False

fifthBitSet ∷ W5 → Bool
fifthBitSet (W5 _ _ _ _ S) = True
fifthBitSet _              = False
