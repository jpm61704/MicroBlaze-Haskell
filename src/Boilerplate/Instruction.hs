module Boilerplate.Instruction where

import Data.Word
import Data.Semigroup

type Name = String
type Size = Word

-- * Definition of Arguments Type

-- | This type is used for defining arguments and their sizes
--   for use as instruction codes
data Arguments = Register Name Arguments
               | Immediate Arguments
               | END
               deriving(Show, Read)

instance Monoid Arguments where
  mempty = END
  mappend = (<>)

instance Semigroup Arguments where
  (Register n xs) <> x = Register n (xs <> x)
  (Immediate xs) <> x = Immediate (xs <> x)
  END <> x = x


data InstructionType = InsType Arguments

typeA :: InstructionType
typeA = InsType (rd <> ra <> rb)

typeB :: InstructionType
typeB = InsType (rd <> ra <> imm)

imm = Immediate END

ra = Register "ra" END

rb = Register "rb" END

rd = Register "rd" END


--Decode




--what i want
{-
mbInsSet = [ "add" <> typeA <> (+)
           , "addi" <> typeB <> (+) ]
-}




{-
processTypeA :: (Monad m) => Word32 -> StateT MicroBlaze m ()
processTypeA w = do
  (ins, rd, ra ,rb) <- parseAFromWord32 w
  -- parseAFromWord32 :: Word32 -> (Ins(with operation), loc rd, loc ra, loc rb)
  a <- get ra
  b <- get rb
  op <- getOp ins
  set rd (a `op` b)
  return ()
-}

-- Maybe on second thought the instructions should not include sizing.
-- Register sizes could vary in size but still operate on the same ins set

