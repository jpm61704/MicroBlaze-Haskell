module Boilerplate.Instruction where

import           Boilerplate.Machines
import           Boilerplate.Machines.State
import qualified Data.Map.Lazy              as M
import           Data.Semigroup
import           Data.Word

-- | The definition of some abstract instruction
data Instruction l sl r m a = Ins { name :: String
                                  , exec :: ExecutionFunction l sl r m a
                                  }

-- | arguments to a command
-- represent a mapping of register argument labels to their register label
-- example: ra -> r1 or rd -> r32
data Arguments l = Args (M.Map l l)

-- | the command that actually executes on a machine
data Command l sl r m a = Command { instruction :: (Instruction l sl r m a)
                                  , arguments   :: (Arguments l) }

getExec :: Command l sl r m a -> ExecutionFunction l sl r m a
getExec (Command ins _) = exec ins

getArgs :: Command l sl r m a -> Arguments l
getArgs (Command _ args) = args

unpackCommand :: Command l sl r m a -> (Arguments l, ExecutionFunction l sl r m a)
unpackCommand (Command ins args) = (args, exec ins)

-- | function that executes on the state given a set of arguments
type ExecutionFunction l sl r m a = (Arguments l -> MachineST l sl r m a)

type InstructionSet l sl r m a = M.Map String (Instruction l sl r m a)

execute :: Command l sl r m a -> MachineST l sl r m a
execute com = let (args, f) = unpackCommand com
              in f args


typeA :: (Monad m) => (r -> r -> r) -> ExecutionFunction String String r m ()
typeA op (Args x) = do
  let (Just ra) = M.lookup "ra" x
      (Just rb) = M.lookup "rb" x
      (Just rd) = M.lookup "rd" x
  operate op ra rb rd


add :: (Monad m, Num r) => Instruction String String r m ()
add = Ins "add" (typeA (+))
