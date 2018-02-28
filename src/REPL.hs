{-# LANGUAGE UnicodeSyntax #-}
-- |

module REPL where

import qualified Boilerplate.W32        as W32
import           InsSet
import           MachineState
import           MachineState.Execution
import           Prelude.Unicode

repl ∷ OutboundSignals → IO (InboundSignals)
repl (Out next_instr_addr r w) = do
  postWrite w
  getRead r
  buildInstruction next_instr_addr
  -- read instruction
  -- encode instruction


  return (⊥)


postWrite ∷ Maybe (Address, StoreData, MBSize) → IO ()
postWrite Nothing          = return ()
postWrite (Just (a, d, s)) = putStrLn $ d' ++ " has been written to memory location " ++ a' ++ " with size " ++ s' ++ "."
      where a' = show $ W32.toInteger a
            d' = show $ W32.toInteger d
            s' = show s


getRead ∷ Maybe (Address, MBReg, MBSize) → IO (Maybe (MBSize, LoadData, MBReg))
getRead Nothing = return Nothing
getRead (Just (a, reg, s)) = do
  let a' = show $ W32.toInteger a
  putStrLn $ "a read has been requested from memory location " ++ a' ++ " to register " ++ (show reg)
  putStrLn "What value should be placed here? (enter in base 10)"
  val ← readLn
  let val' = W32.fromInteger val
  return $ Just (s, val', reg)


buildInstruction ∷ Address → IO (Ins)
buildInstruction x = do
    putStrLn $ "What is the next instruction to be executed? (Location: " ++ (show (W32.toInteger x)) ++ ")"
    x ← getLine
    -- continue here
    
    return (⊥)














