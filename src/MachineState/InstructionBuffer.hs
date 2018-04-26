-- |
{-# LANGUAGE UnicodeSyntax #-}
module MachineState.InstructionBuffer where

import           InsSet
import Data.Word

data InstructionBuffer = InstructionBuffer { _decode  ∷ Maybe Word32
                                           , _execute ∷ Maybe Ins }


type Address = Word32

emptyInstructionBuffer :: InstructionBuffer
emptyInstructionBuffer = InstructionBuffer  Nothing Nothing

