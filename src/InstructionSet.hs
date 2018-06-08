module InstructionSet where

import MachineState
import Boilerplate.Instruction
import Boilerplate.Machines.State
import Boilerplate.IO
import Boilerplate.Execution
import Boilerplate.Machines.SpecialPurposeRegister
import Data.Word
import Boilerplate.Parse
import Parsing
import Data.Semigroup
import Data.Maybe


mbDefinition :: MachineDefinition String String Word32 IO
mbDefinition = MachineDef newMicroBlaze instructionList printMachine parseMBReg

type MBInstruction m = Instruction String String Word32 m

instructionList :: InstructionSet String String Word32 IO
instructionList = setFromList [ Ins "add" formA $ typeA (+)
                              , Ins "addi" formB $ typeB (+)
                              , Ins "addc" formA $ typeA (+)
                              , Ins "addic" formB typeB (+)
                              ]

formA :: ArgumentsForm String
formA = reg "rd" <> reg "ra" <> reg "rb"

formB :: ArgumentsForm String
formB = reg "rd" <> reg "ra" <> imm "imm"

parseMBReg :: Parser String
parseMBReg = do
  space
  char 'r'
  n <- natural
  if n >= 0 && n < 32
    then return $ "r" ++ if n < 10 then '0' : show n else (show n)
    else undefined

data StatusBits = CC
                | VMS
                | VM
                | UMS
                | UM
                | PVR
                | EIP
                | EE
                | DCE
                | DZ
                | ICE
                | FSL
                | BIP
                | C
                | IE
                | BE
                deriving (Show, Eq)

msrBitConf :: BitConfiguration StatusBits
msrBitConf = named CC <>
             blanks 16 <>
             names [VMS, VM, UMS, UM, PVR, EIP, EE, DCE, DZ, ICE, FSL, BIP, C, IE, BE]

testStatus :: (Monad m) => StatusBits -> MachineST l String Word32 m (Maybe Bool)
testStatus = testWithConfig "msr" msrBitConf

-- reader needs to be added to ExecutionFunction so that they compose well
withCarry :: (Monad m) => ExecutionFunction String String Word32 m
withCarry args = do
  let (Just (Register rd)) = getArg args "rd"
  x <- getRegister rd >>= return . fromJust
  c <- testStatus C >>= return . fromJust
  if c
     then setRegister rd (x + 1)
     else return ()


typeA :: (Monad m)
      => (Word32 -> Word32 -> Word32)
      -> ExecutionFunction String String Word32 m
typeA op args = do
  let (Just (Register ra)) = getArg args "ra"
      (Just (Register rb)) = getArg args "rb"
      (Just (Register rd)) = getArg args "rd"
  operate op ra rb rd


typeB :: (Monad m)
      => (Word32 -> Word32 -> Word32)
      -> ExecutionFunction String String Word32 m
typeB op args = do
  let (Just (Register ra)) = getArg args "ra"
      (Just (Immediate imm)) = getArg args "imm"
      (Just (Register rd)) = getArg args "rd"
  operateWithImmediate op ra imm rd
