{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boilerplate.Machines.SpecialPurposeRegister(


  -- *** Bit Configuration

  {-|
The 'BitConfiguration' type represents a naming of bits in a "MachineStatus" Register

This is used to generate utility functions to access and manipulate individual bits with
the notion that they are flags representing different aspects of the instruction set such
as carry, overflow, division by zero, etc.
  -}

  BitConfiguration( BitConf
                  ),
  blank,
  blanks,
  named,
  names,

  getNumber 

                                                  ) where

import Data.Semigroup
import Data.List
import Data.Bits

{-|
A naming configuration for bits in a 'MachineStatus' Register. This provides
name-based access to a bit in an msr based on the bits function as a flag. 
-}
newtype BitConfiguration l = BitConf [Maybe l] deriving (Monoid, Semigroup)

{-|
an unnamed bit

typically used to set aside space for flow related operations, constants, or
non-user accessible flags
-}
blank :: BitConfiguration l
blank = BitConf [Nothing]

{-|
Same as 'blank' but for multiple blanks in order
-}
blanks :: Int -> BitConfiguration l
blanks n = foldr (\n' xs -> (<>) xs blank) mempty [1..n]

{-|
Defines a named bit that represents some flag in a Machine Status Register('MachineStatus').
-}
named :: l -> BitConfiguration l
named name = BitConf [Just name]

{-|
Same as 'named', but, provided a list of names, names the next n bits according to the
provided list. (where n is the number of names provided)
-}
names :: [l] -> BitConfiguration l
names xs = BitConf $ map Just xs

instance (Show l) => Show (BitConfiguration l) where
  show (BitConf xs) = tail $ foldr prnt "" $ zip xs [0 .. (length xs - 1)]
    where prnt (Just name, _) = (++) ("," ++ show name)
          prnt (Nothing, x)    = (++) ("," ++ show x)

getNumber :: (Eq l) => BitConfiguration l -> l -> Maybe Int
getNumber (BitConf config) n = elemIndex (Just n) config

leng :: BitConfiguration l -> Int
leng (BitConf x) = length x












