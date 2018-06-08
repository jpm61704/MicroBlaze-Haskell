{-# LANGUAGE UnicodeSyntax #-}
-- |
module IO where

import           Control.Monad
import           InsSet
import           MachineState
import           MachineState.MachineStatusRegister

-- printRegisters ∷ MicroBlaze → IO ()
-- printRegisters mb = do
--   let p = printRegister mb
--   p R0
--   p R1
--   p R2
--   p R3
--   p R4
--   p R5
--   p R6
--   p R7
--   p R8
--   p R9
--   p R10
--   p R11
--   p R12
--   p R13
--   p R14
--   p R15
--   p R16
--   p R17
--   p R18
--   p R19
--   p R20
--   p R21
--   p R22
--   p R23
--   p R24
--   p R25
--   p R26
--   p R27
--   p R28
--   p R29
--   p R30
--   p R31
--   return ()

-- printRPC ∷ MicroBlaze → IO ()
-- printRPC (MicroBlaze _ rpc _ _) = do
--   putStrLn $ "RPC:\t\t " ++ (show rpc) 


-- printRegister ∷ MicroBlaze → MBReg → IO ()
-- printRegister (MicroBlaze rs _ _ _) reg = do
--   let wd = readRegister reg rs
--   putStr $ show reg ++ ":\t\t "
--   putStrLn $ show wd
--   return ()

-- printMicroBlaze ∷ MicroBlaze → IO ()
-- printMicroBlaze mb@(MicroBlaze _ _ rmsr _) = do
--   printRegisters mb
--   putStrLn "---------------------------------------------------------"
--   printRPC mb
--   putStrLn "---------------------------------------------------------"
--   printStatus rmsr

-- printStatus ∷ RMSR → IO ()
-- printStatus rmsr = mapM_ (\msb → putStrLn ((show msb) ++ ": " ++ (show (getStatus msb rmsr)))) msbs
--   where msbs = [ CarryCopy
--                , DataCacheEnable
--                , DivisionByZero
--                , InstructionCacheEnable
--                , FSLError
--                , BreakInProgress
--                , Carry
--                , InterruptEnable
--                , BuslockEnable
--                , DelayEnable ]
