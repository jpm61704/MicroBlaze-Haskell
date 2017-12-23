module MachineState(
  -- * Microblaze Machine type
    MicroBlaze(MicroBlaze)
  , MBWord

  -- ** Special-Purpose Registers
  , RPC

  -- *** Machine Status Register
  , RMSR( RMSR, _cc, _dce, _dz, _ice, _fsl, _bip, _c, _ie, _be)
  , MachineStatusBit(..)
  , setMSRBit
  , getMSRBit

  -- ** General-Purpose Registers
  , MBRegisters
  -- *** Register Manipulation
  , getRegister
  , setRegister
  ) where

import           Boilerplate
import           Control.Monad.State.Lazy
import           InsSet


-- * Microblaze

-- | Full MicroBlaze Register Profile
data MicroBlaze = MicroBlaze MBRegisters RPC RMSR

-- | Standard word size for MicroBlaze
type MBWord = W32

-- | Program Counter
type RPC = MBWord

-- ** Machine Status Register

-- | the machine status register
data RMSR = RMSR { _cc  :: Bit -- ^ arithmetic carry copy (read-only)
                 , _dce :: Bit -- ^ data cache enable
                 , _dz  :: Bit -- ^ division by zero
                 , _ice :: Bit -- ^ instruction cache enable
                 , _fsl :: Bit -- ^ fsl error
                 , _bip :: Bit -- ^ break in progress
                 , _c   :: Bit -- ^ arithmetic carry
                 , _ie  :: Bit -- ^ interrupt enable
                 , _be  :: Bit -- ^ buslock enable
                 }

data MachineStatusBit = CarryCopy
                      | DataCacheEnable
                      | DivisionByZero
                      | InstructionCacheEnable
                      | FSLError
                      | BreakInProgress
                      | Carry
                      | InterruptEnable
                      | BuslockEnable


-- | sets the machine status bit indicated to the desired boolean value
setStatus :: MachineStatusBit -> Bit -> RMSR -> RMSR
setStatus CarryCopy _ rmsr = rmsr
setStatus DataCacheEnable b (RMSR cc _ dz ice fsl bip c ie be) = (RMSR cc b dz ice fsl bip c ie be)
setStatus DivisionByZero b (RMSR cc dce _ ice fsl bip c ie be) = (RMSR cc dce b ice fsl bip c ie be)
setStatus InstructionCacheEnable b (RMSR cc dce dz _ fsl bip c ie be) = (RMSR cc dce dz b fsl bip c ie be)
setStatus FSLError b (RMSR cc dce dz ice _ bip c ie be) = (RMSR cc dce dz ice b bip c ie be)
setStatus BreakInProgress b (RMSR cc dce dz ice fsl _ c ie be) = (RMSR cc dce dz ice fsl b c ie be)
setStatus Carry b (RMSR _ dce dz ice fsl bip _ ie be) = (RMSR b dce dz ice fsl bip b ie be)
setStatus InterruptEnable b (RMSR cc dce dz ice fsl bip c _ be) = (RMSR cc dce dz ice fsl bip c b be)
setStatus BuslockEnable b (RMSR cc dce dz ice fsl bip c ie _) = (RMSR cc dce dz ice fsl bip c ie b)

getStatus :: MachineStatusBit -> RMSR -> Bit
getStatus CarryCopy              = _cc
getStatus DataCacheEnable        = _dce
getStatus DivisionByZero         = _dz
getStatus InstructionCacheEnable = _ice
getStatus FSLError               = _fsl
getStatus BreakInProgress        = _bip
getStatus Carry                  = _c
getStatus InterruptEnable        = _ie
getStatus BuslockEnable          = _be


setMSRBit :: MachineStatusBit -> Bit -> State MicroBlaze ()
setMSRBit msb b = do
  (MicroBlaze rs rpc rmsr) <- get
  let rmsr' = setStatus msb b rmsr
  put $ MicroBlaze rs rpc rmsr'
  return ()

getMSRBit :: MachineStatusBit -> State MicroBlaze Bit
getMSRBit msb = do
  (MicroBlaze _ _ rmsr) <- get
  return $ getStatus msb rmsr


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
readRegister R0 _                          = zero32
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


getRegister :: MBReg -> State MicroBlaze MBWord
getRegister r = do
  (MicroBlaze x _ _) <- get
  return $ readRegister r x


setRegister :: MBReg -> MBWord -> State MicroBlaze ()
setRegister r w = do
  (MicroBlaze rs rpc msr) <- get
  put $ MicroBlaze (writeRegister r w rs) rpc msr
  return ()
