{-# LANGUAGE UnicodeSyntax#-}
module Boilerplate.W16 where

import qualified Prelude as P
import Boilerplate
import qualified Boilerplate.W8 as W8

fromInteger :: P.Int -> W16
fromInteger x = W16 c0 c1 c2 c3 c4 c5 c6 c7 b0 b1 b2 b3 b4 b5 b6 b7
  where (r0, W8 b0 b1 b2 b3 b4 b5 b6 b7) = w8FromInteger' x
        (_ , W8 c0 c1 c2 c3 c4 c5 c6 c7) = w8FromInteger' r0
        w8FromInteger' = (\x → let y = W8.fromInteger x in ((P.div x (2 P.^ 8)), y))
