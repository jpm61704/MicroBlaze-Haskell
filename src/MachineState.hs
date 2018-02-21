{-|
Module : MachineState
Description : The data representations and basic state manipulations for the MB Processor
-}
{-# LANGUAGE UnicodeSyntax #-}
module MachineState(
  -- * Microblaze Machine type
    MicroBlaze(MicroBlaze)
  , MBWord
  , newMicroBlaze

  -- ** Special-Purpose Registers
  , RPC
  , getRPC
  , setRPC
  -- *** Machine Status Register
  , setMSRBit
  , getMSRBit
  , pullMSR
  , pushMSR
  -- ** General-Purpose Registers
  , MBRegisters
  , emptyRegisters
  -- *** Register Manipulation
  , getRegister
  , setRegister
  , readRegister

  -- ** Memory Managment
  , Address
  -- *** Loading
  , loadWord
  , loadHalfWord
  , loadByte
  -- *** Storing
  , storeWord
  , storeHalfWord
  , storeByte

  -- ** Instruction Buffer
  , fetch
  , pushInstructionBuffer
  , pullDecode
  , pullExec

  ) where

import           MachineState.InstructionBuffer
import           MachineState.MachineStatusRegister

import           Boilerplate
import qualified Boilerplate.W32                    as W32
import           Control.Monad.State.Lazy
import           InsSet


-- * Microblaze

-- | Full MicroBlaze Register Profile
data MicroBlaze = MicroBlaze MBRegisters RPC RMSR InstructionBuffer

-- | Standard word size for MicroBlaze
type MBWord = W32

-- | Creates a zero-initialized MicroBlaze controller
newMicroBlaze :: MicroBlaze
newMicroBlaze = MicroBlaze emptyRegisters W32.zero emptyRMSR emptyInstructionBuffer

-- | Program Counter
type RPC = MBWord

-- | Gets the RPC from a MicroBlaze State
getRPC :: State MicroBlaze MBWord
getRPC = do
  (MicroBlaze _ rpc _ _) <- get
  return rpc

-- | Sets the RPC in a MicroBlaze State
setRPC :: MBWord -> State MicroBlaze ()
setRPC loc = do
  (MicroBlaze rs _ rmsr ib) <- get
  put $ MicroBlaze rs loc rmsr ib
  return ()

-- ** Machine Status Register

-- | Sets a specified bit on the machine status register
setMSRBit :: MachineStatusBit     -- ^ Machine Status Flag to set
          → Bit                  -- ^ The value to set it to
          → State MicroBlaze ()
setMSRBit msb b = do
  (MicroBlaze rs rpc rmsr ib) <- get
  let rmsr' = setStatus msb b rmsr
  put $ MicroBlaze rs rpc rmsr' ib
  return ()

-- | Gets the value of a specified MSR Bit
getMSRBit :: MachineStatusBit -> State MicroBlaze Bit
getMSRBit msb = do
  (MicroBlaze _ _ rmsr ib) <- get
  return $ getStatus msb rmsr

-- | Pulls the enture Machine Status Register as a 32-bit Word
pullMSR ∷ State MicroBlaze W32
pullMSR = do
  (MicroBlaze _ _ rmsr _) ← get
  return $ getMSRWord rmsr

-- | Pushes an enture MSR as a 32-bit word
pushMSR ∷ W32 → State MicroBlaze ()
pushMSR w = do
  delay ← getMSRBit DelayEnable
  let rmsr' = setStatus DelayEnable delay (setMSRWord w)
  (MicroBlaze rs rpc _ ib) ← get
  put $ MicroBlaze rs rpc rmsr' ib



-- ** Register Specifications

-- | the types of registers in MicroBlaze
data MBRegisterType = Dedicated | Volatile | NonVolatile | Special

-- | The register store for the Microblaze
data MBRegisters = MBRegisters RegBlock RegBlock RegBlock RegBlock

data RegBlock = RB MBWord MBWord MBWord MBWord MBWord MBWord MBWord MBWord

data RegBlockPos = RB1
                 | RB2
                 | RB3
                 | RB4
                 | RB5
                 | RB6
                 | RB7
                 | RB8

writeRegBlock :: RegBlockPos -> MBWord -> RegBlock -> RegBlock
writeRegBlock RB1 w (RB _  w2 w3 w4 w5 w6 w7 w8) = RB w w2 w3 w4 w5 w6 w7 w8
writeRegBlock RB2 w (RB w1 _  w3 w4 w5 w6 w7 w8) = RB w1 w w3 w4 w5 w6 w7 w8
writeRegBlock RB3 w (RB w1 w2 _  w4 w5 w6 w7 w8) = RB w1 w2 w w4 w5 w6 w7 w8
writeRegBlock RB4 w (RB w1 w2 w3 _  w5 w6 w7 w8) = RB w1 w2 w3 w w5 w6 w7 w8
writeRegBlock RB5 w (RB w1 w2 w3 w4 _  w6 w7 w8) = RB w1 w2 w3 w4 w w6 w7 w8
writeRegBlock RB6 w (RB w1 w2 w3 w4 w5 _  w7 w8) = RB w1 w2 w3 w4 w5 w w7 w8
writeRegBlock RB7 w (RB w1 w2 w3 w4 w5 w6 _  w8) = RB w1 w2 w3 w4 w5 w6 w w8
writeRegBlock RB8 w (RB w1 w2 w3 w4 w5 w6 w7 _ ) = RB w1 w2 w3 w4 w5 w6 w7 w

readRegBlock :: RegBlockPos -> RegBlock -> MBWord
readRegBlock RB1 (RB w1 _ _ _ _ _ _ _) = w1
readRegBlock RB2 (RB _ w2 _ _ _ _ _ _) = w2
readRegBlock RB3 (RB _ _ w3 _ _ _ _ _) = w3
readRegBlock RB4 (RB _ _ _ w4 _ _ _ _) = w4
readRegBlock RB5 (RB _ _ _ _ w5 _ _ _) = w5
readRegBlock RB6 (RB _ _ _ _ _ w6 _ _) = w6
readRegBlock RB7 (RB _ _ _ _ _ _ w7 _) = w7
readRegBlock RB8 (RB _ _ _ _ _ _ _ w8) = w8


-- | write to a given register
-- note that register zero is persistent
writeRegister :: MBReg -> MBWord -> MBRegisters -> MBRegisters
writeRegister R0 w rs = rs
writeRegister R1 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB2 w b1) b2 b3 b4
writeRegister R2 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB3 w b1) b2 b3 b4
writeRegister R3 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB4 w b1) b2 b3 b4
writeRegister R4 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB5 w b1) b2 b3 b4
writeRegister R5 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB6 w b1) b2 b3 b4
writeRegister R6 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB7 w b1) b2 b3 b4
writeRegister R7 w (MBRegisters b1 b2 b3 b4) = MBRegisters (writeRegBlock RB8 w b1) b2 b3 b4
writeRegister R8 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB1 w b2) b3 b4
writeRegister R9 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB2 w b2) b3 b4
writeRegister R10 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB3 w b2) b3 b4
writeRegister R11 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB4 w b2) b3 b4
writeRegister R12 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB5 w b2) b3 b4
writeRegister R13 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB6 w b2) b3 b4
writeRegister R14 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB7 w b2) b3 b4
writeRegister R15 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 (writeRegBlock RB8 w b2) b3 b4
writeRegister R16 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB1 w b3) b4
writeRegister R17 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB2 w b3) b4
writeRegister R18 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB3 w b3) b4
writeRegister R19 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB4 w b3) b4
writeRegister R20 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB5 w b3) b4
writeRegister R21 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB6 w b3) b4
writeRegister R22 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB7 w b3) b4
writeRegister R23 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 (writeRegBlock RB8 w b3) b4
writeRegister R24 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB1 w b4)
writeRegister R25 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB2 w b4)
writeRegister R26 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB3 w b4)
writeRegister R27 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB4 w b4)
writeRegister R28 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB5 w b4)
writeRegister R29 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB6 w b4)
writeRegister R30 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB7 w b4)
writeRegister R31 w (MBRegisters b1 b2 b3 b4) = MBRegisters b1 b2 b3 (writeRegBlock RB8 w b4)

-- | read a given register
readRegister :: MBReg -> MBRegisters -> MBWord
readRegister R0 _                          = W32.zero
readRegister R1 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB2 b1
readRegister R2 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB3 b1
readRegister R3 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB4 b1
readRegister R4 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB5 b1
readRegister R5 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB6 b1
readRegister R6 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB7 b1
readRegister R7 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB8 b1
readRegister R8 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB1 b2
readRegister R9 (MBRegisters b1 b2 b3 b4)  = readRegBlock RB2 b2
readRegister R10 (MBRegisters b1 b2 b3 b4) = readRegBlock RB3 b2
readRegister R11 (MBRegisters b1 b2 b3 b4) = readRegBlock RB4 b2
readRegister R12 (MBRegisters b1 b2 b3 b4) = readRegBlock RB5 b2
readRegister R13 (MBRegisters b1 b2 b3 b4) = readRegBlock RB6 b2
readRegister R14 (MBRegisters b1 b2 b3 b4) = readRegBlock RB7 b2
readRegister R15 (MBRegisters b1 b2 b3 b4) = readRegBlock RB8 b2
readRegister R16 (MBRegisters b1 b2 b3 b4) = readRegBlock RB1 b3
readRegister R17 (MBRegisters b1 b2 b3 b4) = readRegBlock RB2 b3
readRegister R18 (MBRegisters b1 b2 b3 b4) = readRegBlock RB3 b3
readRegister R19 (MBRegisters b1 b2 b3 b4) = readRegBlock RB4 b3
readRegister R20 (MBRegisters b1 b2 b3 b4) = readRegBlock RB5 b3
readRegister R21 (MBRegisters b1 b2 b3 b4) = readRegBlock RB6 b3
readRegister R22 (MBRegisters b1 b2 b3 b4) = readRegBlock RB7 b3
readRegister R23 (MBRegisters b1 b2 b3 b4) = readRegBlock RB8 b3
readRegister R24 (MBRegisters b1 b2 b3 b4) = readRegBlock RB1 b4
readRegister R25 (MBRegisters b1 b2 b3 b4) = readRegBlock RB2 b4
readRegister R26 (MBRegisters b1 b2 b3 b4) = readRegBlock RB3 b4
readRegister R27 (MBRegisters b1 b2 b3 b4) = readRegBlock RB4 b4
readRegister R28 (MBRegisters b1 b2 b3 b4) = readRegBlock RB5 b4
readRegister R29 (MBRegisters b1 b2 b3 b4) = readRegBlock RB6 b4
readRegister R30 (MBRegisters b1 b2 b3 b4) = readRegBlock RB7 b4
readRegister R31 (MBRegisters b1 b2 b3 b4) = readRegBlock RB8 b4

-- | gets the value at a specified register
getRegister :: MBReg -> State MicroBlaze MBWord
getRegister r = do
  (MicroBlaze x _ _ _) <- get
  return $ readRegister r x

-- | sets the value at a specified register
setRegister :: MBReg -> MBWord -> State MicroBlaze ()
setRegister r w = do
  (MicroBlaze rs rpc msr ib) <- get
  put $ MicroBlaze (writeRegister r w rs) rpc msr ib
  return ()




emptyRB :: RegBlock
emptyRB = RB W32.zero W32.zero W32.zero W32.zero W32.zero W32.zero W32.zero W32.zero

-- | zero-initialized register bank
emptyRegisters :: MBRegisters
emptyRegisters = MBRegisters emptyRB emptyRB emptyRB emptyRB

-- | loads a 32-bit word from memory
--
-- __NOT YET IMPLEMENTED__
loadWord ∷ W32 → State MicroBlaze W32
loadWord = undefined

-- | loads a 16-bit half-word from memory
--
-- __NOT YET IMPLEMENTED__
loadHalfWord ∷ W32 → State MicroBlaze W16
loadHalfWord = undefined

-- | loads a byte from memory
--
-- __NOT YET IMPLEMENTED__
loadByte ∷ W32 → State MicroBlaze W8
loadByte = undefined


-- | stores a 32-bit word in memory
--
-- __NOT YET IMPLEMENTED__
storeWord ∷ W32 → W32 →  W32 → State MicroBlaze ()
storeWord = undefined

-- | stores a 16-bit half-word in memory
--
-- __NOT YET IMPLEMENTED__
storeHalfWord ∷ W16 → W32 → W32 → State MicroBlaze ()
storeHalfWord = undefined

-- | stores a 8-bit byte in memory
--
-- __NOT YET IMPLEMENTED__
storeByte ∷ W8 → W32 → W32 → State MicroBlaze ()
storeByte = undefined




fetch ∷ Address → State MicroBlaze W32
fetch = undefined



pullDecode ∷ State MicroBlaze (Maybe W32)
pullDecode = do
  (MicroBlaze _ _ _ (InstructionBuffer d _)) ← get
  return d

pullExec ∷ State MicroBlaze (Maybe Ins)
pullExec = do
  (MicroBlaze _ _ _ (InstructionBuffer _ e)) ← get
  return e

pushInstructionBuffer ∷ InstructionBuffer → State MicroBlaze ()
pushInstructionBuffer ib = do
  (MicroBlaze x y z _) ← get
  put (MicroBlaze x y z ib)

