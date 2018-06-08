module Boilerplate.Machines
  (
    -- * Machines
    Machine
  , machine
  , registers
    -- * Register Banks
  , RegisterBank
  , newRegisterBank
  , standardRegisterBank
  , getBank
    -- ** Register Manipulation
  , setRegister
  , getRegister
  , modifyRegister
    -- ** Special Purpose Register Banks
  , SpecialRegisters
  , specialFromList 
  , setSpecialRegister
  , getSpecialRegister
  , modifySpecialRegister

  , setSPRBit
  , clearSPRBit
    -- * Examples
    -- ** MicroBlaze
  , mb
  ) where

import           Boilerplate.Machines.SpecialPurposeRegister
import           Data.Bits
import qualified Data.Map.Lazy as M
import           Data.Word

-- * Machine

-- | A machine of inert(stateless) registers
data Machine l sl r = Machine (RegisterBank l r) (SpecialRegisters sl r)
  deriving (Show)

machine :: (FiniteBits r)
        => Int
        -> [ String ]
        -> Machine String String r
machine n sprs = Machine (standardRegisterBank n) (specialFromList sprs)

registers :: Machine l sl r -> RegisterBank l r
registers (Machine rb _) = rb

specialRegisters :: Machine l sl r -> SpecialRegisters sl r
specialRegisters (Machine _ srb) = srb

getRegister :: (Ord l) => l -> Machine l sl r -> Maybe r
getRegister l = (M.lookup l) . getBank . registers

setRegister :: (Ord l) => l -> r -> Machine l sl r -> Machine l sl r
setRegister l r (Machine (RegisterBank m) srb) = Machine (RegisterBank $ M.adjust (const r) l m) srb

modifyRegister :: (Ord l) => l -> (r -> r) -> Machine l sl r -> Machine l sl r
modifyRegister l f m = case getRegister l m of
                         Just r -> setRegister l (f r) m
                         Nothing -> m

getSpecialRegister :: (Ord sl) => sl -> Machine l sl r -> Maybe r
getSpecialRegister sl = (M.lookup sl) . getBank . specialRegisters

setSpecialRegister :: (Ord sl) => sl -> r -> Machine l sl r -> Machine l sl r
setSpecialRegister sl r (Machine rb (RegisterBank m)) = Machine rb (RegisterBank $ M.adjust (const r) sl m)

modifySpecialRegister :: (Ord sl) => sl -> (r -> r) -> Machine l sl r -> Machine l sl r
modifySpecialRegister sl f m = case getSpecialRegister sl m of
                                 Just r -> setSpecialRegister sl (f r) m
                                 Nothing -> m

setSPRBitTo :: (FiniteBits r, Ord sl) => Bool -> sl -> Int -> Machine l sl r -> Machine l sl r
setSPRBitTo b x n m = modifySpecialRegister x (\r -> op r n) m
  where op = if b then setBit else clearBit

setSPRBit :: (FiniteBits r, Ord sl) => sl -> Int -> Machine l sl r -> Machine l sl r
setSPRBit x n m = setSPRBitTo True x n m

clearSPRBit :: (FiniteBits r, Ord sl) => sl -> Int -> Machine l sl r -> Machine l sl r
clearSPRBit x n m = setSPRBitTo False x n m

-- * RegisterBanks

-- | A register bank with labels l and registers r
newtype RegisterBank l r = RegisterBank { getBank :: M.Map l r } deriving Show


-- | Creates a new register bank using a generator functions to produce label l
newRegisterBank :: (FiniteBits r, Ord l)
                => Int                     -- ^ Size of the bank
                -> (Int -> l)               -- ^ Function to generate labels
                -> RegisterBank l r
newRegisterBank size labelmaker = RegisterBank $ foldr insertN M.empty [0..(size - 1)]
  where insertN n rb = M.insert (labelmaker n) zeroBits rb

-- | produces a register bank with numeric labels proceeded by the
--   character 'r'. For example the first register is "r0"
--   Important to note that this produces a strange order from labels
--   being Strings
standardRegisterBank :: (FiniteBits r) => Int -> RegisterBank String r
standardRegisterBank n = newRegisterBank n (\n -> "r" ++ if n < 10 then '0' : (show n) else show n)

-- * Special Purpose Register Banks

type SpecialRegisters sl r = RegisterBank sl r

specialFromList :: (FiniteBits r, Ord l) => [l] -> SpecialRegisters l r
specialFromList ls = newRegisterBank (length ls) (\n -> ls !! n)



-- * Example(MicroBlaze)

mb :: Machine String String Word32
mb = Machine main_bank spec_bank
  where main_bank = standardRegisterBank 32
        spec_bank = specialFromList ["msr", "rpc"]

