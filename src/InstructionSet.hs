module InstructionSet where

import           Boilerplate.Execution
import           Boilerplate.Instruction
import           Boilerplate.IO
import           Boilerplate.Machines.SpecialPurposeRegister
import           Boilerplate.Machines.State
import           Boilerplate.Parse
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Bits
import qualified Data.Map                                    as M
import           Data.Maybe
import           Data.Semigroup
import           Data.Word
import           MachineState
import           Parsing


mbDefinition :: MachineDefinition String String Word32 IO ()
mbDefinition = MachineDef newMicroBlaze instructionList (printMachine (msrBitConf, "msr") "pc") parseMBReg

type MBInstruction m = Instruction String String Word32 m

data MBControl = MBControl { delayFlag :: Bool
                           , loadFlag :: Bool
                           , storeFlag :: Bool
                           }

endOP :: ExecutionFunction String String Word32 IO MBControl
endOP = return $ MBControl False False False

withDelay :: ExecutionFunction String String Word32 IO MBControl
withDelay = return $ MBControl True False False

-- This need to reflect:
-- # immediate storing
-- # load, store
-- # delay flags
instructionList :: InstructionSet String String Word32 IO ()
instructionList = setFromList [ Ins "add" formA    $ add ("ra", "rb") False >>= carryOut
                              , Ins "addc" formA   $ addWithCarryS ("ra", "rb") >>= carryOut
                              , Ins "addk" formA   $ add ("ra", "rb") False >>= keep
                              , Ins "addkc" formA  $ addWithCarryS ("ra", "rb") >>= keep
                              , Ins "addi" formB   $ add ("ra", "rb") False >>= carryOut
                              , Ins "addic" formB  $ addWithCarryS ("ra", "imm") >>= carryOut
                              , Ins "addik" formB  $ add ("ra", "rb") False >>= keep
                              , Ins "addikc" formB $ addWithCarryS ("ra", "imm") >>= keep
                              , Ins "and" formA    $ typeA (.&.)
                              , Ins "andi" formB   $ typeB (.&.)
                              , Ins "andn" formA   $ typeA andn
                              , Ins "andni" formB  $ typeB andn
                              , Ins "beq" formBranchA $ branchIf "ra" (== 0) "rb"
                              , Ins "beqd" formBranchA $ undefined
                              , Ins "beqi" formBranchB $ branchIf "ra" (== 0) "imm"
                              , Ins "beqid" formBranchB $ undefined
                              , Ins "bge" formBranchA $ branchIf "ra" (>= 0) "rb"
                              , Ins "bged" formBranchA $ undefined
                              , Ins "bgei" formBranchB $ branchIf "ra" (>= 0) "imm"
                              , Ins "bgeid" formBranchB $ undefined
                              , Ins "bgt" formBranchA $ branchIf "ra" (> 0) "rb"
                              , Ins "bgtd" formBranchA $ undefined
                              , Ins "bgti" formBranchB $ branchIf "ra" (> 0) "imm"
                              , Ins "bgtid" formBranchB $ undefined
                              , Ins "ble" formBranchA $ branchIf "ra" (<= 0) "rb"
                              , Ins "bled" formBranchA $ undefined
                              , Ins "blei" formBranchB $ branchIf "ra" (<= 0) "imm"
                              , Ins "bleid" formBranchB $ undefined
                              , Ins "blt" formBranchA $ branchIf "ra" (< 0) "rb"
                              , Ins "bltd" formBranchA $ undefined
                              , Ins "blti" formBranchB $ branchIf "ra" (< 0) "imm"
                              , Ins "bltid" formBranchB $ undefined
                              , Ins "bne" formBranchA $ branchIf "ra" (/= 0) "rb"
                              , Ins "bned" formBranchA $ undefined
                              , Ins "bnei" formBranchB $ branchIf "ra" (/= 0) "imm"
                              , Ins "bneid" formBranchB $ undefined 
                              , Ins "cmp" formA    $ compareU "ra" "rb"
                              ]
  where andn = \a b -> a .&. (complement b)



compareU :: (Monad m) => String -> String -> ExecutionFunction String String Word32 m ()
compareU x y = operateOn (\a b -> let d = b - a in if a > b then setBit d 31 else clearBit d 31) (x,y) >>= placeResultAt "rd"



carryAddr :: MSRBitAddress String StatusBits
carryAddr = BitAddr "msr" msrBitConf C

formA :: ArgumentsForm String
formA = reg "rd" <> reg "ra" <> reg "rb"

formB :: ArgumentsForm String
formB = reg "rd" <> reg "ra" <> imm "imm"

formBranchA :: ArgumentsForm String
formBranchA = reg "ra" <> reg "rb"

formBranchB :: ArgumentsForm String
formBranchB = reg "ra" <> imm "imm"

formUnconditionalBranch :: ArgumentsForm String
formUnconditionalBranch = reg "rb"

formUBranchWithLink :: ArgumentsForm String
formUBranchWithLink = reg "rd" <> formUnconditionalBranch

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


getRegisterValues :: (Monad m, Ord l) => [Argument l r] -> MachineST l sl r m (Maybe [r])
getRegisterValues [] = return $ Just []
getRegisterValues (x:xs) = do
  let findVal :: (Monad m, Ord l) => Argument l r -> MachineST l sl r m (Maybe r)
      findVal arg = case arg of
                      Register r    -> getRegister r
                      Immediate imm -> return $ Just imm
  x' <- findVal x
  xs' <- getRegisterValues xs
  return $ maybeCons x' xs'

maybeCons :: Maybe x -> Maybe [x] -> Maybe [x]
maybeCons x xs = do
  x' <- x
  xs' <- xs
  return $ x' : xs'

typeA :: (Monad m) => (Word32 -> Word32 -> Word32) -> ExecutionFunction String String Word32 m ()
typeA op = operateOn op ("ra", "rb") >>= placeResultAt "rd"

typeB :: (Monad m) => (Word32 -> Word32 -> Word32) -> ExecutionFunction String String Word32 m ()
typeB op = operateOn op ("ra", "imm") >>= placeResultAt "rd"


-- PARTIAL
addToPC :: (Monad m) => Word32 -> ExecutionFunction String String Word32 m ()
addToPC x = (lift $ getSpecialRegister "pc") >>= \(Just pc) -> lift $ setSpecialRegister "pc" (x + pc)



destFromAB :: (Monad m)
      => (Word32 -> Word32 -> Word32)
      -> ArgumentsForm String
      -> ExecutionFunction String String Word32 m ()
destFromAB op form = do
  ((Register rd):xs) <- ask >>= \args -> return . fromJust $ unpackFromForm form args
  y <- lift $ getRegisterValues xs
  let (Just (ra:rb:[])) = y
  lift $ setRegister rd (op ra rb)
  return ()

destFromABWithCarry :: (Monad m)
                    => (Word32 -> Word32 -> Bool -> Word32)
                    -> ArgumentsForm String
                    -> ExecutionFunction String String Word32 m ()
destFromABWithCarry op form = do
  ((Register rd):xs) <- ask >>= \args -> return . fromJust $ unpackFromForm form args
  y <- lift $ getRegisterValues xs
  let (Just (ra:rb:[])) = y
  c <- lift $ testWithConfig "msr" msrBitConf C >>= return . fromJust
  placeResultAt rd (op ra rb c)


-- !!! NEED MORE COMPOSABLE FUNCTIONS LIKE THIS
-- | takes a list of argument names (ex: ra, rb, imm, etc.) and returns their
--   values in terms of the current state
getLiteralValuesForArgs :: (Monad m, Ord l) => [l] -> ExecutionFunction l sl r m (M.Map l r)
getLiteralValuesForArgs xs = do
  args <- ask
  let xs' = (fromJust (sequence $ fmap (getArg args) xs))
  vals <- lift $ (forM xs' findVal) >>= return . sequence
  return $ M.fromList $ zip xs (fromJust vals)

findVal :: (Monad m, Ord l) => Argument l r -> MachineST l sl r m (Maybe r)
findVal arg = case arg of
                Register r    -> getRegister r
                Immediate imm -> return $ Just imm

getLiteralValueForArg :: (Monad m, Ord l) => l -> ExecutionFunction l sl r m (Maybe r)
getLiteralValueForArg x = do
  x' <- asks (\as -> getArg as x)
  lift $ findVal (fromJust x')



operateOn :: (Monad m, Ord l)
          => (r -> r -> a)                   -- ^ The operation to perform on the two values
          -> (l, l)                        -- ^ The values to be operated on (in-order of application)
          -> ExecutionFunction l sl r m a
operateOn op (x,y) = do
  xs <- getLiteralValuesForArgs [x, y]
  return $ fromJust $ do
    a <- M.lookup x xs
    b <- M.lookup y xs
    return $ op a b

placeResultAt :: (Monad m, Ord l)
              => l                              -- ^ The register to set in terms of the description
              -> r                              -- ^ The value to set the register to
              -> ExecutionFunction l sl r m ()
placeResultAt dest val = do
  (Just (Register rd)) <- asks $ \args -> getArg args dest
  lift $ setRegister rd val

testCarry :: (Monad m) => ExecutionFunction String String Word32 m Bool
testCarry = lift $ testWithConfig "msr" msrBitConf C >>= \b -> case b of
                                                                Just b' -> return b'
                                                                Nothing -> return False

setCarryTo :: (Monad m) => Bool -> ExecutionFunction String String Word32 m ()
setCarryTo b = lift $ setWithConfigTo "msr" msrBitConf C b

-- | check if overflow will occur from addition
overflow :: (Num r, FiniteBits r, Ord r) => r -> r -> r -> Bool
overflow x y c = z < x || z < y
  where z = x + y + c


addRegular :: (Monad m) => (String, String) -> ExecutionFunction String String Word32 m ()
addRegular xs = add xs False >>= carryOut

add :: (Monad m) => (String, String) -> Bool -> ExecutionFunction String String Word32 m Bool
add xs c = do
  (y,c') <- operateOn (addc c) xs
  placeResultAt "rd" y
  return c'

keep :: (Monad m) => Bool -> ExecutionFunction String String Word32 m ()
keep _ = return ()

-- reader needs to be added to ExecutionFunction so that they compose well
carryOut :: (Monad m) => Bool -> ExecutionFunction String String Word32 m ()
carryOut = setCarryTo

addWithCarryS :: (Monad m) => (String, String) -> ExecutionFunction String String Word32 m Bool
addWithCarryS xs = do
  c <- testCarry
  (y,c') <- operateOn (addc c) xs
  placeResultAt "rd" y
  return c'

addc :: (Num r, FiniteBits r, Ord r) => Bool -> r -> r -> (r, Bool)
addc c x y = (x + y + c', overflow x y c')
  where c' = case c of
               True  -> 1
               False -> 0


withArg :: (Ord l, Monad m) => l -> (r -> ExecutionFunction l sl r m a) -> ExecutionFunction l sl r m a
withArg l f = do
  x <- getLiteralValueForArg l >>= return . fromJust
  f x

with :: (Ord l, Monad m) => (r -> ExecutionFunction l sl r m a) -> l -> ExecutionFunction l sl r m a
f `with` l = withArg l f

with2 :: (Ord l, Monad m) => (r -> r -> ExecutionFunction l sl r m a) -> (l,l) -> ExecutionFunction l sl r m a
f `with2` (l1, l2) = do
  x <- getLiteralValueForArg l1 >>= return . fromJust
  y <- getLiteralValueForArg l2 >>= return . fromJust
  f x y

-- | Check a predicate against an argument's state value
(.?.) :: (Ord l, Monad m) => l -> (r -> ExecutionFunction l sl r m Bool) -> ExecutionFunction l sl r m Bool
(.?.) r p = p `with` r

ifTrue :: (Monad m, Ord l) => ExecutionFunction l sl r m a -> Bool -> ExecutionFunction l sl r m ()
ifTrue f b = if b then f >> return () else return ()

branchIf :: (Monad m) => String -> (Word32 -> Bool) -> String -> ExecutionFunction String String Word32 m ()
branchIf x p j = x .?. (return . p) >>= ifTrue (addToPC `with` j)