{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boilerplate.Instruction where

import           Boilerplate.Machines
import           Boilerplate.Machines.State
import qualified Data.Map.Lazy              as M
import           Data.Semigroup
import           Data.Word
import Control.Monad.Trans.Reader
import Control.Monad

-- | The definition of some abstract instruction
data Instruction l sl r m a = Ins { name :: String
                                , form :: ArgumentsForm l
                                , exec :: ExecutionFunction l sl r m a
                                }

instance Show (Instruction l sl r m a) where
  show (Ins name form exec) = name

data Argument l r = Register l
                  | Immediate r
                  deriving (Read, Show)

-- | arguments to a command
-- represent a mapping of register argument labels to their register label or immediate data
-- example: ra -> r1 or rd -> r32, imm -> 123
data Arguments l r = Args (M.Map l (Argument l r)) deriving Show



-- left is register and right is immediate with their names
newtype ArgumentsForm l = Form [(ArgumentForm, l)] deriving (Monoid, Semigroup)

data ArgumentForm = Reg
                  | Imm

reg :: l -> ArgumentsForm l
reg x = Form [(Reg, x)]

imm :: l -> ArgumentsForm l
imm x = Form [(Imm, x)]

getArg :: (Ord l) => Arguments l r -> l -> Maybe (Argument l r)
getArg (Args m) x = M.lookup x m

unpackArgs :: (Ord l) => Arguments l r -> [l] -> Maybe [Argument l r]
unpackArgs m xs = forM xs (getArg m)

formToList :: ArgumentsForm l -> [l]
formToList (Form ((_,x):xs)) = x : formToList (Form xs)
formToList (Form []) = []

unpackFromForm :: (Ord l) => ArgumentsForm l -> Arguments l r -> Maybe [Argument l r]
unpackFromForm form args = unpackArgs args $ formToList form

-- | the command that actually executes on a machine
data Command l sl r m a = Command { instruction :: (Instruction l sl r m a)
                                  , arguments   :: (Arguments l r) }
                        deriving Show

getExec :: Command l sl r m a -> ExecutionFunction l sl r m a
getExec (Command ins _) = exec ins

getArgs :: Command l sl r m a -> Arguments l r
getArgs (Command _ args) = args

unpackCommand :: Command l sl r m a -> (Arguments l r, ExecutionFunction l sl r m a)
unpackCommand (Command ins args) = (args, exec ins)

-- | function that executes on the state given a set of arguments
type ExecutionFunction l sl r m a = ReaderT (Arguments l r) (MachineST l sl r m ) a

--type ExecutionFunction l sl r m = (Arguments l r -> MachineST l sl r m ())

-- NEED TO COMMIT TO GH BEFORE COMPLETING THIS REFACTOR
-- new ExecutionFunction

type InstructionSet l sl r m a = M.Map String (Instruction l sl r m a)

execute :: Command l sl r m a -> MachineST l sl r m a
execute com = let (args, f) = unpackCommand com
              in runReaderT f args

argsFromList :: (Ord l) => [(l, Argument l r)] -> Arguments l r 
argsFromList xs = Args $ M.fromList xs


setFromList :: [Instruction l sl r m a] -> InstructionSet l sl r m a
setFromList xs = M.fromList xs'
  where xs' = map (\x -> (name x, x)) xs
