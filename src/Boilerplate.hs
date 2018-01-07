{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate where

import qualified Prelude as P

-- * Bit Data

-- | Standard Bit Datatype
-- Supporting functions can be found in Boilerplate.Bit
data Bit = C | S

instance P.Show Bit where
  show C = "0"
  show S = "1"

-- * Byte and Standard Size Data

-- | Standard Byte defintion
-- Supporting functions can be found in Boilerplate.W8
data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving P.Show

data W16 = W16 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving P.Show

data W32 = W32 W8 W8 W8 W8 deriving (P.Show)

-- * Unusual/Rare-Sized Bit Data

data W5  =  W5 Bit Bit Bit Bit Bit deriving P.Show

data W6  =  W6 Bit Bit Bit Bit Bit Bit deriving P.Show

data W11 = W11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit deriving P.Show

data W26 = W26 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit deriving P.Show


