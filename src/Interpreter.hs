{-|
Module : Interpreter
Description: The instruction interpreter for the MicroBlaze processor
-}
{-# LANGUAGE UnicodeSyntax #-}
module Interpreter where


-- import           Control.Monad.Resumption.Reactive
-- import           Control.Monad.State.Lazy
-- import           Data.Bits
-- import           Data.Maybe
-- import           Data.Word
-- import           Decode
-- import           InsSet
-- import           MachineState
-- import           MachineState.InstructionBuffer
-- import           MachineState.MachineStatusRegister

-- executeNext ∷ State MicroBlaze ()
-- executeNext = do
--   maybeIns ← pullExec
--   case maybeIns of
--     (Just ins) → exec ins
--     Nothing    → return ()

-- type MBWord = Word32 
-- -- decodeNext :: State MicroBlaze (Maybe Ins)
-- -- decodeNext = do
-- --   maybeDecode ← pullDecode
-- --   case maybeDecode of
-- --     (Just d) → return $ Just $ decode d
-- --     Nothing  → return Nothing

-- nextPCAddress ∷ State MicroBlaze Address
-- nextPCAddress = do
--   pc ← getRPC
--   setRPC $ pc + 4
--   return pc

-- isPositive :: Word32 -> Bool
-- isPositive x = testBit x 31

-- isNegative :: Word32 -> Bool
-- isNegative = not . isPositive

-- lessThanOrEqualToZero :: Word32 -> Bool
-- lessThanOrEqualToZero x = (not (isPositive x)) || x == 0

-- exec :: (Monad m) => Ins -> StateT MicroBlaze m ()
-- exec (Add rd ra rb)     = add False False (ra, Left rb) rd
-- exec (Addc rd ra rb)    = add True False (ra, Left rb) rd
-- exec (Addk rd ra rb)    = add False True (ra, Left rb) rd
-- exec (Addkc rd ra rb)   = add True True (ra, Left rb) rd
-- exec (Addi rd ra imm)   = add False False (ra, Right imm) rd
-- exec (Addic rd ra imm)  = add True False (ra, Right imm) rd
-- exec (Addik rd ra imm)  = add False True (ra, Right imm) rd
-- exec (Addikc rd ra imm) = add True True (ra, Right imm) rd
-- exec (And rd ra rb)     = execTypeA (.&.) rd ra rb
-- exec (Andi rd ra imm)   = execTypeB (.&.) rd ra imm
-- exec (Andn rd ra rb)    = execTypeA (\a b→ a .&. (complement b)) rd ra rb
-- exec (Andni rd ra imm)  = execTypeB (\a b→ a .&. (complement b)) rd ra imm
-- exec (Beq ra rb )      = branch (TypeA ra rb) ((==) 0)
-- exec (Beqd ra rb )     = delay >> branch (TypeA ra rb ) ((==) 0)
-- exec (Beqi ra imm)      = branch (TypeB ra imm) ((==) 0)
-- exec (Beqid ra imm)     = delay >> branch (TypeB ra imm) ((==) 0)
-- exec (Bge ra rb )      = branch (TypeA ra rb) (\x -> testBit x 31)
-- exec (Bged ra rb )     = delay >> branch (TypeA ra rb) isPositive
-- exec (Bgei ra imm)      = branch (TypeB ra imm) isPositive
-- exec (Bgeid ra imm)     = delay >> branch (TypeB ra imm) isPositive
-- exec (Bgt ra rb )      = branch (TypeA ra rb) (\x -> isPositive x && x /= 0)
-- exec (Bgtd ra rb )     = delay >> branch (TypeA ra rb) (\x -> isPositive x && x /= 0)
-- exec (Bgti ra imm)      = branch (TypeB ra imm) (\x -> isPositive x && x /= 0)
-- exec (Bgtid ra imm)     = delay >> branch (TypeB ra imm) (\x -> isPositive x && x /= 0)
-- exec (Ble ra rb )      = branch (TypeA ra rb) lessThanOrEqualToZero
-- exec (Bled ra rb )     = delay >> branch (TypeA ra rb) lessThanOrEqualToZero
-- exec (Blei ra imm)      = branch (TypeB ra imm) lessThanOrEqualToZero
-- exec (Bleid ra imm)     = delay >> branch (TypeB ra imm) lessThanOrEqualToZero
-- exec (Blt ra rb )      = branch (TypeA ra rb) isNegative
-- exec (Bltd ra rb )     = delay >> branch (TypeA ra rb) isNegative
-- exec (Blti ra imm)      = branch (TypeB ra imm) isNegative
-- exec (Bltid ra imm)     = delay >> branch (TypeB ra imm) isNegative
-- exec (Bne ra rb )      = branch (TypeA ra rb) (\x → not (x == 0))
-- exec (Bned ra rb )     = delay >> branch (TypeA ra rb) (\x → not (x == 0))
-- exec (Bnei ra imm)      = branch (TypeB ra imm) (\x → not (x == 0))
-- exec (Bneid ra imm)     = delay >> branch (TypeB ra imm) (\x → not (x == 0))
-- exec (Br rb)            = branch (TypeA undefined rb) (\x → True)
-- exec (Bra rb)           = absoluteBranch (AbsR rb)
-- exec (Brd rb)           = delay >> branch (TypeA undefined rb) (\x → True)
-- exec (Brad rb)          = delay >> absoluteBranch (AbsR rb)
-- exec (Brld rd rb)       = (link rd) >> delay >> branch (TypeA undefined rb) (\x → True)
-- exec (Brald rd rb)      = (link rd) >> delay >> absoluteBranch (AbsR rb)
-- exec (Bri imm)          = branch (TypeB undefined imm) (\x → True)
-- exec (Brai imm)         = absoluteBranch (AbsI imm)
-- exec (Brid imm)         = delay >> branch (TypeB undefined imm) (\x → True)
-- exec (Braid imm)        = delay >> absoluteBranch (AbsI imm)
-- exec (Brlid rd imm)     = (link rd) >> delay >> branch (TypeB undefined imm) (\x → True)
-- exec (Bralid rd imm)    = (link rd) >> delay >> absoluteBranch (AbsI imm)
-- exec (Brk rd rb)        = link rd >> getRegister rb >>= setRPC >> setMSRBit BreakInProgress True
-- exec (Brki rd imm)      = link rd >> setRPC (fromJust (maybeSignExtend imm)) >> setMSRBit BreakInProgress True
-- exec (Bsrl rd ra rb)    = error $ "barrel shifts not yet implemented"
-- exec (Bsra rd ra rb)    = error $ "barrel shifts not yet implemented"
-- exec (Bsll rd ra rb)    = error $ "barrel shifts not yet implemented"
-- exec (Bsrli rd ra imm)  = error $ "barrel shifts not yet implemented"
-- exec (Bsrai rd ra imm)  = error $ "barrel shifts not yet implemented"
-- exec (Bslli rd ra imm)  = error $ "barrel shifts not yet implemented"
-- exec (Cmp rd ra rb)     = execTypeA undefined rd ra rb
-- exec (Cmpu rd ra rb)    = execTypeA (-) rd ra rb
-- exec (Get rd fslx)      = error $ "fsl instructions not yet implemented"
-- exec (Nget rd fslx)     = error $ "fsl instructions not yet implemented"
-- exec (Cget rd fslx)     = error $ "fsl instructions not yet implemented"
-- exec (Ncget rd fslx)    = error $ "fsl instructions not yet implemented"
-- exec (Idiv rd ra rb)    = error $ "requires hardware divider implementation"
-- exec (Idivu rd ra rb)   = error $ "requires hardware divider implementation"
-- exec (Imm imm)          = setRegister R18 (fromJust (backExtend imm)) -- NOTE: I may want to set a flag here
-- exec (Lbu rd ra rb)     = load LByte rd ra (Register rb)
-- exec (Lbui rd ra imm)   = load LByte rd ra (Immediate imm)
-- exec (Lhu rd ra rb)     = load LHalfWord rd ra (Register rb)
-- exec (Lhui rd ra imm)   = load LHalfWord rd ra (Immediate imm)
-- exec (Lw rd ra rb)      = load LWord rd ra (Register rb)
-- exec (Lwi rd ra imm)    = load LWord rd ra (Immediate imm)
-- exec (Mfs rd rs)        = moveFromSRegister rd rs
-- exec (Mts rs ra )       = moveToSRegister rs ra
-- exec (Mul rd ra rb)     = undefined
-- exec (Muli rd ra imm)   = undefined
-- exec (Or rd ra rb)      = execTypeA (.|.) rd ra rb
-- exec (Ori rd ra imm)    = execTypeB (.|.) rd ra imm
-- exec (Put ra fslx)      = error $ "fsl instructions not yet implemented"
-- exec (Nput ra fslx)     = error $ "fsl instructions not yet implemented"
-- exec (Cput ra fslx)     = error $ "fsl instructions not yet implemented"
-- exec (Ncput ra fslx)    = error $ "fsl instructions not yet implemented"
-- exec (Rsub rd ra rb)    = sub False False (ra, Left rb) rd
-- exec (Rsubc rd ra rb)   = sub True False (ra, Left rb) rd
-- exec (Rsubk rd ra rb)   = sub False True (ra, Left rb) rd
-- exec (Rsubkc rd ra rb)  = sub True True (ra, Left rb) rd
-- exec (Rsubi rd ra imm)  = sub False False (ra, Right imm) rd
-- exec (Rsubic rd ra imm) = sub True False (ra, Right imm) rd
-- exec (Rsubik rd ra imm) = sub False True (ra, Right imm) rd
-- exec (Rsubikc rd ra imm) = sub True True (ra, Right imm) rd
-- exec (Rtbd ra imm)      = returnFrom ra imm >> setMSRBit BreakInProgress False
-- exec (Rtid ra imm)      = returnFrom ra imm >> setMSRBit InterruptEnable True
-- exec (Rtsd ra imm)      = returnFrom ra imm
-- exec (Sb rd ra rb)      = store SByte rd ra (Register rb)
-- exec (Sbi rd ra imm)    = store SByte rd ra (Immediate imm)
-- exec (Sext8 rd ra)      = sext8 rd ra
-- exec (Sext16 rd ra)     = sext16 rd ra
-- exec (Sh rd ra rb)      = store SHalfWord rd ra (Register rb)
-- exec (Shi rd ra imm)    = store SHalfWord rd ra (Immediate imm)
-- exec (Sra rd ra)        = shiftRightArithmetic False rd ra
-- exec (Src rd ra)        = shiftRightArithmetic True rd ra
-- exec (Srl rd ra)        = shiftRightLogical rd ra
-- exec (Sw rd ra rb)      = store SWord rd ra (Register rb)
-- exec (Swi rd ra imm)    = store SWord rd ra (Immediate imm)
-- exec (Wdc _ _)        = error "cache instructions not available"
-- exec (Wic _ _)          = error "cache instructions not available"
-- exec (Xor rd ra rb)     = execTypeA xor rd ra rb
-- exec (Xori rd ra imm)   = execTypeB xor rd ra imm
-- exec ins = error $ "instruction " ++ (show ins) ++ " not yet implemented"


-- -- * Utility Functions and Data Types

-- addWithCarry ∷ (Num a, Ord a) => a → a → Bool → (Bool, a)
-- addWithCarry x y ci = (carry_out, sum)
--   where sum = x + y + carry_in
--         carry_out = sum < x || sum < y
--         carry_in  = if ci then 1 else 0


-- -- | Delay Flag for Branching
-- type DelayFlag = Bool


-- -- | Branch Input Type
-- data BranchInput = TypeA MBReg MBReg
--                  | TypeB MBReg Word16

-- -- | Absolute Branch Input Type
-- data AbsoluteBranchInput = AbsR MBReg
--                          | AbsI Word16

-- -- | Isomorphism type
-- type Op = (Word32 → Word32 → Word32)

-- -- ** Generic Operator Execution

-- -- | execute a type a instruction using basic operators
-- execTypeA ∷ (Monad m)
--          => Op                   -- ^ Operator to apply to values
--           → MBReg                -- ^ Destination register
--           → MBReg                -- ^ Input Register A
--           → MBReg                -- ^ Input Register B
--           → StateT MicroBlaze m ()
-- execTypeA op rd ra rb = do
--   a ← getRegister ra
--   b ← getRegister rb
--   setRegister rd $ op a b

-- -- | execute a type b instruction using basic operators
-- execTypeB ∷ (Monad m)
--          => Op                   -- ^ Operator to apply to values
--           → MBReg                -- ^ Destination register
--           → MBReg                -- ^ Input Register A
--           → Word16                  -- ^ Immediate Data
--           → StateT MicroBlaze m ()
-- execTypeB op dest ra imm = do
--   a ← getRegister ra
--   let b = fromJust $ maybeSignExtend imm
--   setRegister dest $ op a b


-- -- ** Branching

-- -- | SHOULD BE DEPRECATED
-- getBranchInputValue ∷ (Monad m) => BranchInput → StateT MicroBlaze m Word32
-- getBranchInputValue (TypeA _ rb)  = getRegister rb
-- getBranchInputValue (TypeB _ w16) = return $ fromJust $ maybeSignExtend w16

-- -- | SHOULD BE DEPRECATED
-- getBranchRegisterA ∷ (Monad m) => BranchInput → StateT MicroBlaze m Word32
-- getBranchRegisterA (TypeA ra _) = getRegister ra
-- getBranchRegisterA (TypeB ra _) = getRegister ra

-- -- | branch to an absolute address
-- absoluteBranch ∷ (Monad m) => AbsoluteBranchInput → StateT MicroBlaze m ()
-- absoluteBranch (AbsR rb) = do
--   b ← getRegister rb
--   setRPC b
-- absoluteBranch (AbsI imm) = setRPC $ fromJust $ maybeSignExtend imm

-- -- | branch to a relative address
-- branch ∷ (Monad m)
--       => BranchInput          -- ^ The branch input type (TypeA vs TypeB)
--        → (MBWord → Bool)      -- ^ The predicate to decide branching
--        → StateT MicroBlaze m ()
-- branch input branch_test = do
--   a ← getBranchRegisterA input
--   case branch_test a of
--     False -> return ()
--     True -> do
--       b ← getBranchInputValue input
--       pc ← getRPC
--       setRPC $ snd $ addWithCarry b pc False

-- -- ** Addition

-- -- | adding mechanism for MicroBlaze (likely can be deprecated)
-- add :: (Monad m) => CarryFlag → KeepFlag → (MBReg, Either MBReg Word16) → MBReg → StateT MicroBlaze m ()
-- add carry keep (ra, y) rd = do
--   a ← getRegister ra
--   b ← case y of
--         Left rb   → getRegister rb
--         Right imm → return $ fromJust $ maybeSignExtend imm
--   c ← if carry then getMSRBit Carry else return False
--   let (carry_out, output) = addWithCarry a b c
--   setRegister rd output
--   if keep
--     then return ()
--     else setMSRBit Carry carry_out

-- -- | hardware subtraction (likely can be deprecated)
-- sub :: (Monad m) => CarryFlag → KeepFlag → (MBReg, Either MBReg Word16) → MBReg → StateT MicroBlaze m ()
-- sub carry keep (ra, y) rd = do
--   a ← getRegister ra
--   b ← case y of
--         Left rb   → getRegister rb
--         Right imm → return $ fromJust $ maybeSignExtend imm
--   c ← if carry then getMSRBit Carry else return False
--   let (carry_out, output) = reverseSubtraction a b c
--   setRegister rd output
--   if keep
--     then return ()
--     else setMSRBit Carry carry_out

-- reverseSubtraction :: (Num a, Bits a, Ord a) => a -> a -> Bool -> (Bool, a)
-- reverseSubtraction x y ci = addWithCarry y (complement x) ci

-- -- | Carry Flag for adder
-- type CarryFlag = Bool

-- -- | Keep Flag for adder
-- type KeepFlag = Bool


-- -- ** Loading From Memory

-- data LoadSize = LWord | LHalfWord | LByte

-- -- | Either-like datatype to differentiate TypeA and TypeB data
-- data ImmOrReg  = Register  MBReg
--                | Immediate Word16

-- -- | Loads data from memory, The two register offsets are added to obtain an address
-- load :: (Monad m)
--      => LoadSize                 -- ^ the size of the load operation (Byte, HalfWord, Word)
--      -> MBReg                    -- ^ Destination register for loaded data
--      → MBReg                    -- ^ Register Offset 1
--      → ImmOrReg                 -- ^ Register Offset 2
--      → StateT MicroBlaze m ()
-- load s rd ra y = do
--   a ← getRegister ra
--   b ← case y of
--         Register rb   → getRegister rb
--         Immediate imm → return $ fromJust $ maybeSignExtend imm
--   let address = snd $ addWithCarry a b False
--   val ← case s of
--               LWord            → loadWord address
--               LHalfWord → do
--                 x ← loadHalfWord address
--                 return $ fromJust $ maybeSignExtend x
--               LByte     → do
--                 x ← loadByte address
--                 return $ fromJust $ maybeSignExtend x
--   setRegister rd val

-- data StoreSize = SWord | SHalfWord | SByte

-- store ∷ (Monad m)
--      => StoreSize
--       → MBReg
--       → MBReg
--       → ImmOrReg
--       → StateT MicroBlaze m ()
-- store s rd ra y = do
--   a ← getRegister ra
--   b ← case y of
--         Register rb   → getRegister rb
--         Immediate imm → return $ fromJust $ (maybeSignExtend imm)
--   d ← getRegister rd
--   case s of
--     SWord     → storeWord d a b
--     SHalfWord → storeHalfWord (leastSignificantHalfWord d) a b
--     SByte     → storeByte (leastSignificantByte d) a b


-- -- ** Special Purpose Registers

-- -- | pulls either the MSR or PC register into given register
-- moveFromSRegister ∷ (Monad m)
--                   => MBReg                   -- ^ Destination Register
--                   → MBSReg                  -- ^ Special Purpose Register to Pull (MSR or RPC)
--                   → StateT MicroBlaze m ()
-- moveFromSRegister rd rs = do
--   sreg ← case rs of
--            MSR → pullMSR
--            RPC → getRPC
--   setRegister rd sreg

-- -- | puts a Word into a special purpose register.
-- -- Does not support updates to the program counter
-- moveToSRegister ∷ (Monad m)
--                => MBSReg                -- ^ The register to alter
--                 → MBReg                 -- ^ The register containing the MSR Word
--                 → StateT MicroBlaze m ()
-- moveToSRegister RPC _  = error "Illegal op: Cannot set program counter using MTS)"
-- moveToSRegister MSR ra = getRegister ra >>= pushMSR


-- -- | links the current program counter value into the specified register
-- link ∷ (Monad m)
--     => MBReg                     -- ^ The destination register for the PC word
--      → StateT MicroBlaze m ()
-- link rd = do
--   pc ← getRPC
--   setRegister rd pc


-- -- | sets the delay flag in the machine status register
-- delay ∷ (Monad m) => StateT MicroBlaze m ()
-- delay = setMSRBit DelayEnable True


-- -- | returns the pc from a break, interrupt, or subroutine
-- returnFrom ∷ (Monad m) => MBReg → Word16 → StateT MicroBlaze m ()
-- returnFrom ra imm = do
--   a ← getRegister ra
--   let b = fromJust $ maybeSignExtend imm
--   setRPC $ snd (addWithCarry a b False)

-- sext8 ∷ (Monad m) => MBReg → MBReg → StateT MicroBlaze m ()
-- sext8 rd ra = do
--   a ← getRegister ra
--   setRegister rd $ fromJust $ maybeSignExtend $ leastSignificantByte a

-- sext16 ∷ (Monad m) => MBReg → MBReg → StateT MicroBlaze m ()
-- sext16 rd ra = do
--   a ← getRegister ra
--   setRegister rd $ fromJust $ maybeSignExtend $ leastSignificantHalfWord a

-- shiftRightArithmetic ∷ (Monad m) => CarryFlag → MBReg → MBReg → StateT MicroBlaze m ()
-- shiftRightArithmetic carry_flag rd ra = do
--   a ← getRegister ra
--   c ← if carry_flag then getMSRBit Carry else return False
--   let (carry, d) = arithmeticShiftRight a c
--   setMSRBit Carry carry
--   setRegister rd d

-- shiftRightLogical ∷ (Monad m) => MBReg → MBReg → StateT MicroBlaze m ()
-- shiftRightLogical rd ra = do
--   a ← getRegister ra
--   let (carry, d) = logicalShiftRight a
--   setMSRBit Carry carry
--   setRegister rd d

-- logicalShiftRight :: (Bits a) => a -> (Bool, a)
-- logicalShiftRight x = (testBit x 0, shiftR x 1)


-- arithmeticShiftRight :: (FiniteBits a) => a -> Bool -> (Bool, a)
-- arithmeticShiftRight x ci = if ci then (co, setBit lsr msb) else x'
--   where msb          = (finiteBitSize x) - 1
--         x'@(co, lsr) = logicalShiftRight x

-- leastSignificantHalfWord :: Word32 -> Word16
-- leastSignificantHalfWord x = foldr testset 0 [0..15]
--   where testset = \n y -> if testBit x n then setBit y n else y

-- leastSignificantByte :: Word32 -> Word8
-- leastSignificantByte x = foldr testset 0 [0..7]
--   where testset = \n y -> if testBit x n then setBit y n else y



-- maybeSignExtend :: (Integral a, Integral b, FiniteBits a, FiniteBits b)
--                 => a
--                 -> Maybe b
-- maybeSignExtend x = do
--   x' <- toIntegralSized x
--   let origional_size = (finiteBitSize x)
--       msb = testBit x (origional_size - 1)
--   return $ if msb
--              then foldr (\i n -> setBit n i) x' [origional_size..(finiteBitSize x')]
--              else x'

-- showFiniteBits :: (FiniteBits a) => a -> String
-- showFiniteBits x = foldr (visualTestBit) "" $ reverse [0..((finiteBitSize x) - 1)]
--   where visualTestBit = \i str -> if testBit x i then "1" ++ str else "0" ++ str

-- -- untested
-- backExtend :: (Integral a, Integral b, FiniteBits a, FiniteBits b) => a -> Maybe b
-- backExtend x = do
--   x' <- toIntegralSized x
--   let size_x  = finiteBitSize x
--       size_x' = finiteBitSize x'
--       diff    = size_x' - size_x
--   return $ shiftL x' diff



