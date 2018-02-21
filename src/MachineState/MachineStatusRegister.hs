{-|
Module : MachineState.MachineStatusRegister

-}
{-# LANGUAGE UnicodeSyntax #-}
module MachineState.MachineStatusRegister
  (
    -- * Machine Status Register
    RMSR( RMSR, _cc, _dce, _dz, _ice, _fsl, _bip, _c, _ie, _be)
  , emptyRMSR
  , getMSRWord
  , setMSRWord
    -- ** Status Flags
  , MachineStatusBit(..)
  , getStatus
  , setStatus
  ) where

import           Boilerplate
import qualified Boilerplate.W8           as W8
import           Control.Monad.State.Lazy


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
                 , _de  ∷ Bit -- ^ delay enable (hidden)
                 }

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

-- | sets the machine status bit indicated to the desired boolean value
setStatus :: MachineStatusBit -> Bit -> RMSR -> RMSR
setStatus CarryCopy _ rmsr = rmsr
setStatus DataCacheEnable b (RMSR cc _ dz ice fsl bip c ie be de) = (RMSR cc b dz ice fsl bip c ie be de)
setStatus DivisionByZero b (RMSR cc dce _ ice fsl bip c ie be de) = (RMSR cc dce b ice fsl bip c ie be de)
setStatus InstructionCacheEnable b (RMSR cc dce dz _ fsl bip c ie be de) = (RMSR cc dce dz b fsl bip c ie be de)
setStatus FSLError b (RMSR cc dce dz ice _ bip c ie be de) = (RMSR cc dce dz ice b bip c ie be de)
setStatus BreakInProgress b (RMSR cc dce dz ice fsl _ c ie be de) = (RMSR cc dce dz ice fsl b c ie be de)
setStatus Carry b (RMSR _ dce dz ice fsl bip _ ie be de) = (RMSR b dce dz ice fsl bip b ie be de)
setStatus InterruptEnable b (RMSR cc dce dz ice fsl bip c _ be de) = (RMSR cc dce dz ice fsl bip c b be de)
setStatus BuslockEnable b (RMSR cc dce dz ice fsl bip c ie _ de) = (RMSR cc dce dz ice fsl bip c ie b de)
setStatus DelayEnable b (RMSR cc dce dz ice fsl bip c ie be _) = (RMSR cc dce dz ice fsl bip c ie be b)

-- | Get the status of a specified flag
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
getStatus DelayEnable            = _de

-- | a zero-initialized (All False) MSR
emptyRMSR :: RMSR
emptyRMSR = RMSR C C C C C C C C C C

-- | gets the status register in the form of a 32-bit word
getMSRWord ∷ RMSR → W32
getMSRWord (RMSR cc dce dz ice fsl bip c ie be _) = W32 (W8 cc C C C C C C C) W8.zero W8.zero (W8 dce dz ice fsl bip c ie be)

-- | sets the status register from a 32-bit MSR word
setMSRWord ∷ W32 → RMSR
setMSRWord (W32 (W8 cc C C C C C C C) _ _ (W8 dce dz ice fsl bip c ie be)) = RMSR cc dce dz ice fsl bip c ie be C
