{-# LANGUAGE UnicodeSyntax #-}
-- |

module MachineState.Execution where

import           Control.Monad.Resumption
import           Control.Monad.Resumption.Reactive
import           Control.Monad.State.Lazy
import           Decode
import           InsSet
import           Interpreter
import           MachineState
import           Prelude.Unicode
import Data.Word
import Data.Maybe

data InboundSignals = In { _readData    ∷ Maybe (MBSize, LoadData, MBReg)
                         , _instruction ∷ Maybe Ins
                         }

data OutboundSignals = Out { _nextInstruction ∷ Address
                           , _read            ∷ Maybe (Address, MBReg, MBSize)
                           , _write           ∷ Maybe (Address, StoreData, MBSize)
                           , _st             :: MicroBlaze }

type StoreData = Word32
type LoadData  = Word32


type MBlazeRe m = ReacT InboundSignals OutboundSignals (StateT MicroBlaze m)

startFDE :: (Monad m) => MBlazeRe m ()
startFDE = fde $ In Nothing Nothing

fde ∷ (Monad m) => InboundSignals → MBlazeRe m () 
fde i = do
  st ← lift get
  lift $ processIncomingMemory i
  m_ins ← case _instruction i of
                Just raw_ins → do
                  -- let ins =  decode raw_ins -- unsafe
                  lift $ exec raw_ins
                  return (Just raw_ins)
                Nothing      → return Nothing
  x ← lift $ makeOutbound m_ins
  lift $ incrementPC
  i' ← signal x
  fde i'

incrementPC ∷ (Monad m) => StateT MicroBlaze m ()
incrementPC = do
  pc ← getRPC
  let (c, pc') = addWithCarry pc 4 False
  setRPC pc'

makeOutbound :: (Monad m) => Maybe Ins → StateT MicroBlaze m OutboundSignals
makeOutbound Nothing      = do
  x <- get
  return $ Out 0 Nothing Nothing x
makeOutbound (Just ins)   = liftM4 Out next_ins o_read o_write o_state
  where next_ins = getRPC
        o_write  = processWrite ins
        o_read   = processLoad ins
        o_state  = get

processWrite ∷ (Monad m) => Ins → StateT MicroBlaze m (Maybe (Address, StoreData, MBSize))
processWrite ins = do
  case unpackWrite ins of
    Just (rD, rA, Right rB, size) → do
      a ← getRegister rA
      b ← getRegister rB
      d ← getRegister rD
      return $ Just (snd (addWithCarry a b False), d, size)
    Just (rD, rA, Left imm, size) → do
      a ← getRegister rA
      let b = fromJust $ maybeSignExtend imm
      d ← getRegister rD
      return $ Just (snd (addWithCarry a b False), d, size)
    Nothing → return Nothing



unpackWrite ∷ Ins → Maybe (MBReg, MBReg, Either Word16 MBReg, MBSize)
unpackWrite ins = case ins of
  Sb rD rA rB   → Just (rD, rA, Right rB, ByteSize)
  Sbi rD rA imm → Just (rD, rA, Left imm, ByteSize)
  Sh rD rA rB   → Just (rD, rA, Right rB, HalfWordSize)
  Shi rD rA imm → Just (rD, rA, Left imm, HalfWordSize)
  Sw rD rA rB   → Just (rD, rA, Right rB, WordSize)
  Swi rD rA imm → Just (rD, rA, Left imm, WordSize)
  _             → Nothing


processLoad ∷ (Monad m) => Ins → StateT MicroBlaze m (Maybe (Address, MBReg, MBSize))
processLoad ins = do
  case unpackLoad ins of
    Just (rD, rA, Right rB, size) → do
      a ← getRegister rA
      b ← getRegister rB
      return $ Just (snd (addWithCarry a b False), rD, size)
    Just (rD, rA, Left imm, size) → do
      a ← getRegister rA
      return $ Just (snd (addWithCarry a (fromJust $ maybeSignExtend imm) False), rD, size)
    Nothing                       → return Nothing


unpackLoad ∷ Ins → Maybe (MBReg, MBReg, Either Word16 MBReg, MBSize)
unpackLoad ins = case ins of
  Lbu rD rA rB   → Just (rD, rA, Right rB, ByteSize)
  Lhu rD rA rB   → Just (rD, rA, Right rB, HalfWordSize)
  Lw  rD rA rB   → Just (rD, rA, Right rB, WordSize)
  Lbui rD rA imm → Just (rD, rA, Left imm, ByteSize)
  Lhui rD rA imm → Just (rD, rA, Left imm, HalfWordSize)
  Lwi rD rA imm  → Just (rD, rA, Left imm, WordSize)
  _              → Nothing


processIncomingMemory ∷ (Monad m) => InboundSignals → StateT MicroBlaze m ()
processIncomingMemory (In (Just (size, w, rD)) _) = case size of
  WordSize     → setRegister rD w
  HalfWordSize → (⊥)
  ByteSize     → (⊥)
processIncomingMemory _                               = return ()

