 {-|
Module : InsSet
Description : Data definitions for the MicroBlaze Instruction Set
-}
module InsSet where

import           Boilerplate
import           Data.Int
import Data.Word

-- | Possible data sizes
data MBSize = ByteSize
            | HalfWordSize
            | WordSize
            deriving Show

-- * Register Type

-- | All user-accesible general-purpose registers
data MBReg = R0 -- ^ Dedicated, Value 0
           | R1 -- ^ Dedicated, Stack Pointer
           | R2 -- ^ Dedicated, Read-only small data area anchor
           | R3 -- ^ Volatile, Return Value
           | R4 -- ^ Volatile, Return Value
           | R5 -- ^ Volatile, Passing Parameter / Temporary
           | R6 -- ^ Volatile, Passing Parameter / Temporary
           | R7 -- ^ Volatile, Passing Parameter / Temporary
           | R8 -- ^ Volatile, Passing Parameter / Temporary
           | R9 -- ^ Volatile, Passing Parameter / Temporary
           | R10 -- ^ Volatile, Passing Parameter / Temporary
           | R11 -- ^ Volatile, Temporary
           | R12 -- ^ Volatile, Temporary
           | R13 -- ^ Dedicated, Read-write small data area anchor
           | R14 -- ^ Dedicated, Return address for Interrupt
           | R15 -- ^ Dedicated, Return address for Sub-routine
           | R16 -- ^ Dedicated, Return address for Trap (Debugger)
           | R17 -- ^ Dedicated, Return address for Exceptions
           | R18 -- ^ Dedicated, Reserved for assembler, used for Imm instruction
           | R19 -- ^ Non-volatile, must be saved across function calls
           | R20 -- ^ Non-volatile, must be saved across function calls
           | R21 -- ^ Non-volatile, must be saved across function calls
           | R22 -- ^ Non-volatile, must be saved across function calls
           | R23 -- ^ Non-volatile, must be saved across function calls
           | R24 -- ^ Non-volatile, must be saved across function calls
           | R25 -- ^ Non-volatile, must be saved across function calls
           | R26 -- ^ Non-volatile, must be saved across function calls
           | R27 -- ^ Non-volatile, must be saved across function calls
           | R28 -- ^ Non-volatile, must be saved across function calls
           | R29 -- ^ Non-volatile, must be saved across function calls
           | R30 -- ^ Non-volatile, must be saved across function calls
           | R31 -- ^ Non-volatile, must be saved across function calls
           deriving (Show,Read,Enum)

--
-- the Microblaze instruction set;
--
-- register arguments appear in the same order as in the well-formed
-- assembly statement;
--

-- | Special Purpose Registers
data MBSReg = RPC             -- ^ Program Counter
            | MSR             -- ^ Machine Status Register
            deriving Show

-- | The MicroBlaze Instruction Set
data Ins  = Add MBReg MBReg MBReg
          | Addc MBReg MBReg MBReg
          | Addk MBReg MBReg MBReg
          | Addkc MBReg MBReg MBReg
          | Addi MBReg MBReg Word16
          | Addic MBReg MBReg Word16
          | Addik MBReg MBReg Word16
          | Addikc MBReg MBReg Word16
          | And MBReg MBReg MBReg
          | Andi MBReg MBReg Word16
          | Andn MBReg MBReg MBReg
          | Andni MBReg MBReg Word16
          | Beq MBReg MBReg 
          | Beqd MBReg MBReg 
          | Beqi MBReg Word16
          | Beqid MBReg Word16
          | Bge MBReg MBReg 
          | Bged MBReg MBReg 
          | Bgei MBReg Word16
          | Bgeid MBReg Word16
          | Bgt MBReg MBReg 
          | Bgtd MBReg MBReg 
          | Bgti MBReg Word16
          | Bgtid MBReg Word16
          | Ble MBReg MBReg 
          | Bled MBReg MBReg 
          | Blei MBReg Word16
          | Bleid MBReg Word16
          | Blt MBReg MBReg 
          | Bltd MBReg MBReg 
          | Blti MBReg Word16
          | Bltid MBReg Word16
          | Bne MBReg MBReg 
          | Bned MBReg MBReg 
          | Bnei MBReg Word16
          | Bneid MBReg Word16
          | Br MBReg
          | Bra MBReg
          | Brd MBReg
          | Brad MBReg
          | Brld MBReg MBReg
          | Brald MBReg MBReg
          | Bri Word16
          | Brai Word16
          | Brid Word16
          | Braid Word16
          | Brlid MBReg Word16
          | Bralid MBReg Word16
          | Brk MBReg MBReg
          | Brki MBReg Word16
          | Bsrl MBReg MBReg MBReg
          | Bsra MBReg MBReg MBReg
          | Bsll MBReg MBReg MBReg
          | Bsrli MBReg MBReg Word16
          | Bsrai MBReg MBReg Word16
          | Bslli MBReg MBReg Word16
          | Cmp MBReg MBReg MBReg
          | Cmpu MBReg MBReg MBReg
{-        | Fadd MBReg MBReg MBReg
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
-}
          -- get from the FSL interface:
          | Get MBReg Int
          | Nget MBReg Int
          | Cget MBReg Int
          | Ncget MBReg Int

--        | Getd MBReg Int
          | Idiv MBReg MBReg MBReg
          | Idivu MBReg MBReg MBReg
          | Imm Word16
          | Lbu MBReg MBReg MBReg
          | Lbui MBReg MBReg Word16
          | Lhu MBReg MBReg MBReg
          | Lhui MBReg MBReg Word16
          | Lw MBReg MBReg MBReg
          | Lwi MBReg MBReg Word16
          | Mfs MBReg MBSReg
--        | Msrclr MBReg Word16
--        | Msrset MBReg Word16
          | Mts MBSReg MBReg
          | Mul MBReg MBReg MBReg
          | Mulhu MBReg MBReg MBReg
          | Mulhsu MBReg MBReg MBReg
          | Muli MBReg MBReg Word16
          | Or MBReg MBReg MBReg
          | Ori MBReg MBReg Word16
          | Pcmpbf MBReg MBReg MBReg
          | Pcmpne MBReg MBReg MBReg
          -- put to FSL interface:
          | Put MBReg Int
          | Nput MBReg Int
          | Cput MBReg Int
          | Ncput MBReg Int
          | Rsub MBReg MBReg MBReg
          | Rsubc MBReg MBReg MBReg
          | Rsubk MBReg MBReg MBReg
          | Rsubkc MBReg MBReg MBReg
          | Rsubi MBReg MBReg Word16
          | Rsubic MBReg MBReg Word16
          | Rsubik MBReg MBReg Word16
          | Rsubikc MBReg MBReg Word16
          | Rtbd MBReg Word16
          | Rtid MBReg Word16
          | Rted MBReg Word16
          | Rtsd MBReg Word16
          | Sb MBReg MBReg MBReg
          | Sbi MBReg MBReg Word16
          | Sext8 MBReg MBReg
          | Sext16 MBReg MBReg
          | Sh MBReg MBReg MBReg
          | Shi MBReg MBReg Word16
          | Sra MBReg MBReg
          | Src MBReg MBReg
          | Srl MBReg MBReg
          | Sw MBReg MBReg MBReg
          | Swi MBReg MBReg Word16
          | Wdc MBReg MBReg
          | Wic MBReg MBReg
          | Xor MBReg MBReg MBReg
          | Xori MBReg MBReg Word16
          deriving (Show)
