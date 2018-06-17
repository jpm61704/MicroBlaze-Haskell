module Boilerplate.IO where

import Data.Bits
import Data.Word
import Boilerplate.Machines
import Boilerplate.Machines.SpecialPurposeRegister
import Data.List.Split
import Text.Printf
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

printMachine :: (Show l, FiniteBits r, Show r, Integral r, Show bsl, Ord sl) => (BitConfiguration bsl, sl) -> sl -> Machine l sl r -> IO ()
printMachine (bconf, msr_label) pc m = do
  printRegisterBank (registers m)
  printProgramCounter pc m
  case getSpecialRegister msr_label m of
    Just msr -> printMSR bconf msr
    Nothing -> putStrLn "couldnt find msr"
  return ()

printProgramCounter :: (Ord sl, Show r) => sl -> Machine l sl r -> IO ()
printProgramCounter pc m = (putStrLn . show . fromJust . (getSpecialRegister pc)) m

printRegisterBank :: (Show l, FiniteBits r, Show r, Integral r) => RegisterBank l r -> IO ()
printRegisterBank b = do
  let m = getBank b
      l = M.toList m
  forM l (\(n, r) -> printRegister (show n) r)
  return ()


printSpecialRegisters :: (Show sl) => SpecialRegisters sl r -> IO ()
printSpecialRegisters = undefined


printRegister :: (FiniteBits r, Show r, Integral r) => String -> r -> IO ()
printRegister n r = putStrLn $ prettyReg n r

prettyReg :: (FiniteBits r, Show r, Integral r) => String -> r -> String
prettyReg n r = printf "%.5s\t%20d %40s" n i (prettybits r)
  where i = fromJust (toIntegralSized r) :: Int

prettybits :: (FiniteBits r) => r -> String
prettybits x = mconcat bs'
  where n = finiteBitSize x
        (h:bs) = chunksOf 8 $ foldr (\i str -> str ++ (if testBit x i then "1" else "0")) "" [0..(n-1)]
        bs' = h : (map ('.' : ) bs)

test1 :: IO ()
test1 = printRegisterBank rb
  where rb :: RegisterBank String Word32
        rb = newRegisterBank 16 (\n -> "r" ++ if n < 10 then "0" ++ (show n) else show n)


printMSR :: (FiniteBits r, Show bsl) => BitConfiguration bsl -> r -> IO ()
printMSR conf msr = putStrLn $ show $ nameNumPairs conf msr
