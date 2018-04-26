{-|
Module : MachineState.MachineStatusRegister

-}
{-# LANGUAGE UnicodeSyntax #-}
module MachineState.MachineStatusRegister
  (
    -- * Machine Status Register
    RMSR
  , emptyRMSR
  , getMSRWord
  , setMSRWord
    -- ** Status Flags
  , MachineStatusBit(..)
  , getStatus
  , setStatus
  ) where

import           Control.Monad.State.Lazy
import Data.Word
import Data.Bits



data RMSR = RMSR Word32

-- | Various status flags in the Machine State Register
data MachineStatusBit = CarryCopy
                      | DataCacheEnable
                      | DivisionByZero
                      | InstructionCacheEnable
                      | FSLError
                      | BreakInProgress
                      | Carry
                      | InterruptEnable
                      | BuslockEnable
                      | DelayEnable
                      deriving Show

msrBitNumber :: MachineStatusBit -> Int
msrBitNumber CarryCopy              = 0 
msrBitNumber DataCacheEnable        = 24
msrBitNumber DivisionByZero         = 25
msrBitNumber InstructionCacheEnable = 26
msrBitNumber FSLError               = 27
msrBitNumber BreakInProgress        = 28
msrBitNumber Carry                  = 29 
msrBitNumber InterruptEnable        = 30
msrBitNumber BuslockEnable          = 31
msrBitNumber DelayEnable            = 1

-- | gets the status register in the form of a 32-bit word
getMSRWord ∷ RMSR → Word32
getMSRWord (RMSR w) = w


-- | sets the status register from a 32-bit MSR word
setMSRWord ∷ Word32 → RMSR
setMSRWord x = (RMSR x) 



-- | sets the machine status bit indicated to the desired boolean value
setStatus :: MachineStatusBit -> Bool -> RMSR -> RMSR
setStatus msb True (RMSR rmsr) = RMSR $ setBit rmsr (msrBitNumber msb)
setStatus msb False (RMSR rmsr) = RMSR $ clearBit rmsr (msrBitNumber msb)

-- | Get the status of a specified flag
getStatus :: MachineStatusBit -> RMSR -> Bool
getStatus msb (RMSR rmsr) = testBit rmsr (msrBitNumber msb) 

-- | a zero-initialized (All False) MSR
emptyRMSR :: RMSR
emptyRMSR = RMSR 0 

