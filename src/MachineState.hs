{-|
Module : MachineState
Description : The data representations and basic state manipulations for the MB Processor
-}
{-# LANGUAGE UnicodeSyntax #-}
module MachineState(
  -- * Microblaze Machine type
    MicroBlaze
  , newMicroBlaze
  ) where

import           Boilerplate.Machines
import           Boilerplate.Machines.State
import           Control.Monad.State.Lazy
import           Data.Word
import           InsSet

-- * Microblaze

-- | Full MicroBlaze Register Profile

type MicroBlaze = Machine String String Word32

type MicroBlazeST m a = MachineST String String Word32 m a

-- | Creates a zero-initialized MicroBlaze controller

newMicroBlaze :: MicroBlaze
newMicroBlaze = machine 32 ["pc", "msr"]

