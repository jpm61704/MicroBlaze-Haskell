{-# LANGUAGE UnicodeSyntax #-}
-- |

module REPL where

import           InsSet
import           MachineState
import           MachineState.Execution
import           Prelude.Unicode
import ParserMicroBlaze
import Control.Monad.State.Lazy
import IO
import Control.Monad.Resumption.Reactive

-- runFreshRepl = runStateT (runReacT startFDE repl) newMicroBlaze

-- repl ∷ OutboundSignals → StateT MicroBlaze IO (InboundSignals)
-- repl (Out next_instr_addr r w st) = do
--   lift $ printMicroBlaze st
--   lift $ postWrite w
--   rd <- lift $ getRead r
--   ins <- lift $ buildInstruction next_instr_addr
--   return $ In rd (Just ins)



-- postWrite ∷ Maybe (Address, StoreData, MBSize) → IO ()
-- postWrite Nothing          = return ()
-- postWrite (Just (a, d, s)) = putStrLn $ d' ++ " has been written to memory location " ++ a' ++ " with size " ++ s' ++ "."
--       where a' = show a
--             d' = show d
--             s' = show s


-- getRead ∷ Maybe (Address, MBReg, MBSize) → IO (Maybe (MBSize, LoadData, MBReg))
-- getRead Nothing = return Nothing
-- getRead (Just (a, reg, s)) = do
--   let a' = show a
--   putStrLn $ "a read has been requested from memory location " ++ a' ++ " to register " ++ (show reg)
--   putStrLn "What value should be placed here? (enter in base 10)"
--   val ← readLn
--   let val' = val
--   return $ Just (s, val', reg)


-- buildInstruction ∷ Address → IO (Ins)
-- buildInstruction x = do
--     putStrLn $ "What is the next instruction to be executed? (Location: " ++ (show x) ++ ")"
--     x <- parseREPL
--     case x of
--       (ins:_) -> return ins
--       _ -> error "bad input"
    
    














