{-# LANGUAGE UnicodeSyntax #-}
module Boilerplate where

import qualified Prelude                as P

import           Control.Monad.Identity hiding (when)
import           Control.Monad.State    hiding (when)

import qualified Boilerplate.Bit        as B
import qualified Boilerplate.W32        as W32
import qualified Boilerplate.W8         as W8

--
-- new boilerplate for DLX.
--



data W5  =  W5 B.Bit B.Bit B.Bit B.Bit B.Bit deriving P.Show

data W6  =  W6 B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit deriving P.Show

data W11 = W11 B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit deriving P.Show

data W16 = W16 B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit deriving P.Show

data W26 = W26 B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit
               B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit
               B.Bit B.Bit B.Bit B.Bit B.Bit B.Bit deriving P.Show

zero16 = W16 B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C B.C


signExtendW16 ∷ W16 → W32.W32
signExtendW16 w@(W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15) =
  W32.W32 byte0 byte0 (W8.W8 b0 b1 b2 b3 b4 b5 b6 b7) (W8.W8 b8 b9 b10 b11 b12 b13 b14 b15)
  where byte0 = W8.W8 b0 b0 b0 b0 b0 b0 b0 b0
