 {-|
Module : InsSet
Description : Data definitions for the MicroBlaze Instruction Set
-}
module InsSet where

import           Boilerplate
import           Boilerplate.Bit as B
import           Boilerplate.W32 as W32
import           Data.Int

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
          | Imm W16
          | Lbu MBReg MBReg MBReg
          | Lbui MBReg MBReg W16
          | Lhu MBReg MBReg MBReg
          | Lhui MBReg MBReg W16
          | Lw MBReg MBReg MBReg
          | Lwi MBReg MBReg W16
          | Mfs MBReg MBSReg
--        | Msrclr MBReg W16
--        | Msrset MBReg W16
          | Mts MBSReg MBReg
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
          | Nput MBReg Int
          | Cput MBReg Int
          | Ncput MBReg Int
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
          | Sext8 MBReg MBReg
          | Sext16 MBReg MBReg
          | Sh MBReg MBReg MBReg
          | Shi MBReg MBReg W16
          | Sra MBReg MBReg
          | Src MBReg MBReg
          | Srl MBReg MBReg
          | Sw MBReg MBReg MBReg
          | Swi MBReg MBReg W16
          | Wdc MBReg MBReg
          | Wic MBReg MBReg
          | Xor MBReg MBReg MBReg
          | Xori MBReg MBReg W16
          deriving (Show)
