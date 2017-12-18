module InsSet where

import Boilerplate
import Data.Int

type Label = String

{-
data ASM = MBS Ins ASM
            | MBL Label ASM
            | MBNil
            deriving Show

mbinscat :: ASM -> ASM -> ASM
mbinscat (MBS i is) is' = MBS i (mbinscat is is')
mbinscat (MBL l is) is' = MBL l (mbinscat is is')
mbinscat MBNil is'      = is'
-}


--
-- Microblaze words:
--
data MBVal = Byte Int8
           | HalfWord Int16
           | Word Int32
           | Label Label
           deriving Show


zerow :: MBVal
zerow = Word 0

onew :: MBVal
onew = Word 1

data MBSize = ByteSize
            | HalfWordSize
            | WordSize
            deriving Show

--
-- Microblaze registers:
--
data MBReg = R0
           | R1
           | R2
           | R3
           | R4
           | R5
           | R6
           | R7
           | R8
           | R9
           | R10
           | R11
           | R12
           | R13
           | R14
           | R15
           | R16
           | R17
           | R18
           | R19
           | R20
           | R21
           | R22
           | R23
           | R24
           | R25
           | R26
           | R27
           | R28
           | R29
           | R30
           | R31
           deriving (Show,Enum)

--
-- the Microblaze instruction set;
--
-- register arguments appear in the same order as in the well-formed
-- assembly statement;
--

{-
data Ins = InsA OpCodeA MBReg MBReg MBReg
         | InsB OpCodeB MBReg MBReg W16

data OpCodeA = Add | Addc | Addk | Addkc | And | Andn
             | Beq | Beqd 

data OpCodeB = Addi | Addic | Addik | Addikc | Andi | Andni
             | Beqi | Beqid 
             | Bge MBReg MBReg
          | Bged MBReg MBReg
          | Bgei MBReg W16
             | Bgeid MBReg W16
             | Bgt MBReg MBReg
             | Bgtd MBReg MBReg
             | Bgti MBReg W16
             | Bgtid MBReg W16
             | Ble MBReg MBReg
             | Bled MBReg MBReg
             | Blei MBReg W16
             | Bleid MBReg W16
             | Blt MBReg MBReg
             | Bltd MBReg MBReg
             | Blti MBReg W16
             | Bltid MBReg W16
             | Bne MBReg MBReg
             | Bned MBReg MBReg
             | Bnei MBReg W16
             | Bneid MBReg W16
             | Br MBReg
             | Bra MBReg
             | Brd MBReg
             | Brad MBReg
             | Brld MBReg MBReg
             | Brald MBReg MBReg

             | Bri W16
             | Brai W16
             | Brid W16
             | Braid W16
             | Brlid MBReg W16
             | Bralid MBReg W16

             | Brk MBReg MBReg
             | Brki MBReg W16

             | Bsrl MBReg MBReg MBReg
             | Bsra MBReg MBReg MBReg
             | Bsll MBReg MBReg MBReg

             | Bsrli MBReg MBReg W16
             | Bsrai MBReg MBReg W16
             | Bslli MBReg MBReg W16

             | Cmp MBReg MBReg MBReg
             | Cmpu MBReg MBReg MBReg

             | Fadd MBReg MBReg MBReg
             | Frsub MBReg MBReg MBReg

             | Fmul MBReg MBReg MBReg
             | Fdiv MBReg MBReg MBReg

             -- the following seven need a '.' before the last two characters
             -- in the code when put to formatted strings:
             | Fcmpun MBReg MBReg MBReg
             | Fcmplt MBReg MBReg MBReg
             | Fcmpeq MBReg MBReg MBReg
             | Fcmple MBReg MBReg MBReg
             | Fcmpgt MBReg MBReg MBReg
             | Fcmpne MBReg MBReg MBReg
             | Fcmpge MBReg MBReg MBReg

             | Flt MBReg MBReg
             | Fint MBReg MBReg
             | Fsqrt MBReg MBReg

             -- get from the FSL interface:
             | Get MBReg Int
             | Getd MBReg Int

             | Idiv MBReg MBReg MBReg
             | Idivu MBReg MBReg MBReg

             | Imm

             | Lbu MBReg MBReg MBReg
             | Lbui MBReg MBReg W16
             | Lhu MBReg MBReg MBReg
             | Lhui MBReg MBReg W16
             | Lw MBReg MBReg MBReg
             | Lwi MBReg MBReg W16

             | Mfs MBReg MBReg
             | Msrclr MBReg W16
             | Msrset MBReg W16
             | Mts MBReg W16

             | Mul MBReg MBReg MBReg
             | Mulhu MBReg MBReg MBReg
             | Mulhsu MBReg MBReg MBReg
             | Muli MBReg MBReg W16

             | Or MBReg MBReg MBReg
             | Ori MBReg MBReg W16

             | Pcmpbf MBReg MBReg MBReg
             | Pcmpne MBReg MBReg MBReg

             -- put to FSL interface:
             | Put MBReg Int
             | Putd MBReg MBReg Int
             | Rsub MBReg MBReg MBReg
             | Rsubc MBReg MBReg MBReg
             | Rsubk MBReg MBReg MBReg
             | Rsubkc MBReg MBReg MBReg

             | Rsubi MBReg MBReg W16
             | Rsubic MBReg MBReg W16
             | Rsubik MBReg MBReg W16
             | Rsubikc MBReg MBReg W16

             | Rtbd MBReg W16
             | Rtid MBReg W16
             | Rted MBReg W16
             | Rtsd MBReg W16

             | Sb MBReg MBReg MBReg
             | Sbi MBReg MBReg W16
             | Sext16 MBReg MBReg

             | Sh MBReg MBReg MBReg
             | Shi MBReg MBReg W16

             | Sra MBReg MBReg
             | Src MBReg MBReg
             | Sw MBReg MBReg MBReg
             | Swi MBReg MBReg W16

             | Wdc MBReg MBReg
             | Wic MBReg MBReg

             | Xor MBReg MBReg MBReg
             | Xori MBReg MBReg W16
             deriving Show
-}

data Ins  = Add MBReg MBReg MBReg
          | Addc MBReg MBReg MBReg
          | Addk MBReg MBReg MBReg
          | Addkc MBReg MBReg MBReg
          | Addi MBReg MBReg W16
          | Addic MBReg MBReg W16
          | Addik MBReg MBReg W16
          | Addikc MBReg MBReg W16
          | And MBReg MBReg MBReg
          | Andi MBReg MBReg W16
          | Andn MBReg MBReg MBReg
          | Andni MBReg MBReg W16
          | Beq MBReg MBReg W11
          | Beqd MBReg MBReg W11
          | Beqi MBReg W16
          | Beqid MBReg W16
          | Bge MBReg MBReg W11
          | Bged MBReg MBReg W11
          | Bgei MBReg W16
          | Bgeid MBReg W16
          | Bgt MBReg MBReg W11
          | Bgtd MBReg MBReg W11
          | Bgti MBReg W16
          | Bgtid MBReg W16
          | Ble MBReg MBReg W11
          | Bled MBReg MBReg W11
          | Blei MBReg W16
          | Bleid MBReg W16
          | Blt MBReg MBReg W11
          | Bltd MBReg MBReg W11
          | Blti MBReg W16
          | Bltid MBReg W16
          | Bne MBReg MBReg W11
          | Bned MBReg MBReg W11
          | Bnei MBReg W16
          | Bneid MBReg W16
          | Br MBReg
          | Bra MBReg
          | Brd MBReg
          | Brad MBReg
          | Brld MBReg MBReg
          | Brald MBReg MBReg
          | Bri W16
          | Brai W16
          | Brid W16
          | Braid W16
          | Brlid MBReg W16
          | Bralid MBReg W16
          | Brk MBReg MBReg
          | Brki MBReg W16
          | Bsrl MBReg MBReg MBReg
          | Bsra MBReg MBReg MBReg
          | Bsll MBReg MBReg MBReg
          | Bsrli MBReg MBReg W16
          | Bsrai MBReg MBReg W16
          | Bslli MBReg MBReg W16
          | Cmp MBReg MBReg MBReg
          | Cmpu MBReg MBReg MBReg
          | Fadd MBReg MBReg MBReg
          | Frsub MBReg MBReg MBReg
          | Fmul MBReg MBReg MBReg
          | Fdiv MBReg MBReg MBReg

          -- the following seven need a '.' before the last two characters
          -- in the code when put to formatted strings:
          | Fcmpun MBReg MBReg MBReg
          | Fcmplt MBReg MBReg MBReg
          | Fcmpeq MBReg MBReg MBReg
          | Fcmple MBReg MBReg MBReg
          | Fcmpgt MBReg MBReg MBReg
          | Fcmpne MBReg MBReg MBReg
          | Fcmpge MBReg MBReg MBReg

          | Flt MBReg MBReg
          | Fint MBReg MBReg
          | Fsqrt MBReg MBReg
          
          -- get from the FSL interface:
          | Get MBReg Int
          | Getd MBReg Int

          | Idiv MBReg MBReg MBReg
          | Idivu MBReg MBReg MBReg

          | Imm

          | Lbu MBReg MBReg MBReg
          | Lbui MBReg MBReg W16
          | Lhu MBReg MBReg MBReg
          | Lhui MBReg MBReg W16
          | Lw MBReg MBReg MBReg
          | Lwi MBReg MBReg W16

          | Mfs MBReg MBReg
          | Msrclr MBReg W16
          | Msrset MBReg W16
          | Mts MBReg W16

          | Mul MBReg MBReg MBReg
          | Mulhu MBReg MBReg MBReg
          | Mulhsu MBReg MBReg MBReg
          | Muli MBReg MBReg W16

          | Or MBReg MBReg MBReg
          | Ori MBReg MBReg W16

          | Pcmpbf MBReg MBReg MBReg
          | Pcmpne MBReg MBReg MBReg

          -- put to FSL interface:
          | Put MBReg Int
          | Putd MBReg MBReg Int
          | Rsub MBReg MBReg MBReg
          | Rsubc MBReg MBReg MBReg
          | Rsubk MBReg MBReg MBReg
          | Rsubkc MBReg MBReg MBReg

          | Rsubi MBReg MBReg W16
          | Rsubic MBReg MBReg W16
          | Rsubik MBReg MBReg W16
          | Rsubikc MBReg MBReg W16

          | Rtbd MBReg W16
          | Rtid MBReg W16
          | Rted MBReg W16
          | Rtsd MBReg W16

          | Sb MBReg MBReg MBReg
          | Sbi MBReg MBReg W16
          | Sext16 MBReg MBReg

          | Sh MBReg MBReg MBReg
          | Shi MBReg MBReg W16

          | Sra MBReg MBReg
          | Src MBReg MBReg
          | Sw MBReg MBReg MBReg
          | Swi MBReg MBReg W16

          | Wdc MBReg MBReg
          | Wic MBReg MBReg

          | Xor MBReg MBReg MBReg
          | Xori MBReg MBReg W16
          deriving Show
