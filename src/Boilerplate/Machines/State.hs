module Boilerplate.Machines.State where

import qualified Boilerplate.Machines      as M
import           Control.Monad.State.Lazy

-- | Stateful Machine Definitions
-- This should be an internal type only
type MachineST l sl r m a = StateT (M.Machine l sl r) m a


-- * Stateful Register Manipulation

setRegister :: (Monad m, Ord l) => l -> r -> MachineST l sl r m ()
setRegister l r = modify $ M.setRegister l r

getRegister :: (Monad m, Ord l) => l -> MachineST l sl r m (Maybe r)
getRegister l = gets $ M.getRegister l

modifyRegister :: (Monad m, Ord l) => l -> (r -> r) -> MachineST l sl r m ()
modifyRegister l f = modify $ M.modifyRegister l f

-- | Conducts an operation on two register values and places the result in
--   a third register r_d
operate :: (Monad m, Ord l) => (r -> r -> r) -> l -> l -> l -> MachineST l sl r m ()
operate op l_1 l_2 l_d = do
  r_1 <- getRegister l_1
  r_2 <- getRegister l_2
  case (r_1, r_2) of
    (Just r_a, Just r_b) -> setRegister l_d (r_a `op` r_b)
    _                    -> return ()

