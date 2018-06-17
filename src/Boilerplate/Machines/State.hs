module Boilerplate.Machines.State where

import qualified Boilerplate.Machines      as M
import Boilerplate.Machines.SpecialPurposeRegister
import           Control.Monad.State.Lazy
import Data.Bits 
import Control.Monad.Trans.Maybe
import Data.Maybe

-- | Stateful Machine Definitions
-- This should be an internal type only
type MachineST l sl r = StateT (M.Machine l sl r)


-- * Stateful Register Manipulation

setRegister :: (Monad m, Ord l) => l -> r -> MachineST l sl r m ()
setRegister l r = modify $ M.setRegister l r

getRegister :: (Monad m, Ord l) => l -> MachineST l sl r m (Maybe r)
getRegister l = gets $ M.getRegister l

modifyRegister :: (Monad m, Ord l) => l -> (r -> r) -> MachineST l sl r m ()
modifyRegister l f = modify $ M.modifyRegister l f

-- | Conducts an operation on two register values and places the result in
--   a third register r_d
operate :: (Monad m, Ord l) => (r -> r -> r) -> l -> l -> l -> MachineST l sl r m r
operate op l_1 l_2 l_d = do
  r_1 <- getRegister l_1
  r_2 <- getRegister l_2
  case (r_1, r_2) of
    (Just r_a, Just r_b) -> do
      let y = (r_a `op` r_b)
      setRegister l_d y
      return y
    _                    -> undefined

operateWithImmediate :: (Monad m, Ord l) => (r -> r -> r) -> l -> r -> l -> MachineST l sl r m ()
operateWithImmediate op l_1 imm l_d = do
  r_1 <- getRegister l_1
  case r_1 of
    Just r_a -> setRegister l_d (r_a `op` imm)
    _        -> return ()

getMultipleRegs :: (Monad m, Ord l) => [l] -> MachineST l sl r m (Maybe [r])
getMultipleRegs ls = forM ls getRegister >>= return . sequence

-- * Stateful Special Purpose Registers

setSpecialRegister :: (Monad m, Ord sl) => sl -> r -> MachineST l sl r m ()
setSpecialRegister sl r = modify $ M.setSpecialRegister sl r

getSpecialRegister :: (Monad m, Ord sl) => sl -> MachineST l sl r m (Maybe r)
getSpecialRegister sl = gets $ M.getSpecialRegister sl

modifySpecialRegister :: (Monad m, Ord sl) => sl -> (r -> r) -> MachineST l sl r m ()
modifySpecialRegister sl f = modify $ M.modifySpecialRegister sl f

setSPRBit :: (Monad m, FiniteBits r, Ord sl) => sl -> Int -> MachineST l sl r m ()
setSPRBit sl n = modify $ M.setSPRBit sl n

clearSPRBit :: (Monad m, FiniteBits r, Ord sl) => sl -> Int -> MachineST l sl r m ()
clearSPRBit sl n = modify $ M.clearSPRBit sl n

setWithConfigTo :: (Eq bsl, Monad m, FiniteBits r, Ord sl)
                => sl
                -> BitConfiguration bsl
                -> bsl
                -> Bool
                -> MachineST l sl r m ()
setWithConfigTo sreg config label b = do
    case getNumber config label of
      Just n
        | b         -> setSPRBit sreg n
        | otherwise -> clearSPRBit sreg n
      Nothing -> return ()

setWithConfig :: (Eq bsl, Monad m, FiniteBits r, Ord sl)
              => sl
              -> BitConfiguration bsl
              -> bsl
              -> MachineST l sl r m ()
setWithConfig = \sreg config label -> setWithConfigTo sreg config label True

clearWithConfig :: (Eq bsl, Monad m, FiniteBits r, Ord sl)
                => sl
                -> BitConfiguration bsl
                -> bsl
                -> MachineST l sl r m ()
clearWithConfig = \sreg config label -> setWithConfigTo sreg config label False

testWithConfig :: (Eq bsl, Monad m, FiniteBits r, Ord sl)
               => sl
               -> BitConfiguration bsl
               -> bsl
               -> MachineST l sl r m (Maybe Bool)
testWithConfig sreg conf x = do
  reg <- getSpecialRegister sreg
  return $ do
    r <- reg
    n <- getNumber conf x
    return $ testBit r n

-- ** Operations on Machine Status Registers 

-- add :: (FiniteBits r, Monad m, Ord l, Num r, Ord r) => r -> r -> l -> MachineST l sl r m Bool
-- add ra rb ld = do
--   let d = ra + rb
--   setRegister ld d
--   return $ ra > d || rb > d

addWithCarry :: (FiniteBits r, Monad m, Ord l, Ord sl, Eq bsl, Num r, Ord r) => MSRBitAddress sl bsl -> r -> r -> l -> MachineST l sl r m Bool
addWithCarry (BitAddr msr conf c) ra rb ld = do
  mc_in <- testWithConfig msr conf c
  case (mc_in) of
    (Just cin) -> do
      let d = ra + rb + (if cin then 1 else 0)
      setRegister ld d
      return $ ra > d || rb > d
    _ -> return False

applyCarry :: (Monad m, FiniteBits r, Ord sl, Eq bsl) => MSRBitAddress sl bsl -> Bool -> MachineST l sl r m ()
applyCarry (BitAddr msr conf c) cout = setWithConfigTo msr conf c cout

-- 3 bit
-- 000 + 111 = 111 no carry
-- 001 + 001 = 010 no carry
-- 111 + 001 = 000 carry


data MSRBitAddress sl bsl = BitAddr sl (BitConfiguration bsl) bsl


-- * Program Counter

-- (>=>) :: Monad m => (sl -> (MachineST l sl r m) (Maybe r))
--                  -> (Maybe r -> (MachineST l sl r m) r) -> a -> m c

branch :: (Monad m, Ord sl, Num r) => sl -> r -> MachineST l sl r m ()
branch pc jump = (getSpecialRegister >=> return . (+ jump) . fromJust) pc >>= setSpecialRegister pc

