{-|
Module : Boilerplate
Description : Contains definitions of useful fixed sized data types
-}
{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate where

import qualified Prelude as P
import Data.Word
import Data.Bits

-- * Bit Data

-- | Standard Bit Datatype
-- Supporting functions can be found in "Boilerplate.Bit"
data Bit = C | S

-- | show instance of Bit displays bits as 1 and 0
instance P.Show Bit where
  show C = "0"
  show S = "1"

-- * Byte and Standard Size Data

-- | Standard Byte defintion
-- Supporting functions can be found in "Boilerplate.W8"
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit

instance P.Show W8 where
  show (W8 b0 b1 b2 b3 b4 b5 b6 b7) = P.concatMap P.show [b0, b1, b2, b3, b4, b5, b6, b7]

-- | 16-bit data
data W16 = W16 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving P.Show

-- | 32-bit data
-- Supporting functions can be found in "Boilerplate.W32"
data W32 = W32 W8 W8 W8 W8

instance P.Show W32 where
  show (W32 b0 b1 b2 b3) = P.concatMap P.show [b0, b1, b2, b3]

-- * Unusual/Rare-Sized Bit Data

-- | 5-bit data
data W5  =  W5 Bit Bit Bit Bit Bit deriving P.Show

-- | 6-bit data
data W6  =  W6 Bit Bit Bit Bit Bit Bit deriving P.Show

-- | 11-bit data
data W11 = W11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving P.Show

maybeSignExtend :: (P.Integral a, P.Integral b, FiniteBits a, FiniteBits b)
                => a
                -> P.Maybe b
maybeSignExtend x = do
  x' <- toIntegralSized x
  let origional_size = (finiteBitSize x)
      msb = testBit x (origional_size P.- 1)
  P.return P.$ if msb
             then P.foldr (\i n -> setBit n i) x' [origional_size..(finiteBitSize x')]
             else x' 

showFiniteBits :: (FiniteBits a) => a -> P.String
showFiniteBits x = P.foldr (visualTestBit) "" P.$ P.reverse [0..((finiteBitSize x) P.- 1)]
  where visualTestBit = \i str -> if testBit x i then "1" P.++ str else "0" P.++ str
