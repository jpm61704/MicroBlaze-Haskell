module Boilerplate.Execution where

import Boilerplate.Instruction
import Boilerplate.Machines
import Boilerplate.Machines.State
import Boilerplate.Parse 
import Control.Monad.Resumption.Reactive
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Parsing

type In l sl r m a = Maybe (Command l sl r m a)
type Out = String

type MachineReacT l sl r m a =
  ReacT
  (In l sl r m a)
  Out 
  (MachineST l sl r m)
  a

data MachineDefinition l sl r m a = MachineDef { machine :: Machine l sl r
                                               , insSet  :: InstructionSet l sl r m a
                                               , prnt :: (Machine l sl r -> IO ())
                                               , _parse :: Parser l
                                               }

runMachine :: (Integral r, Ord l, Read l)
           => MachineDefinition l sl r IO a
           -> IO ()
runMachine (MachineDef mach insset prt prs) = do
  runStateT (runReacT startBasicFDE (basicREPL insset prs prt)) mach
  return ()

-- runReacT :: Monad m => ReacT input output m a -> (output -> m input) -> m a


startBasicFDE :: (Monad m) => MachineReacT l sl r m a
startBasicFDE = basicFDE Nothing

-- starting with FDE with no memory anything
basicFDE :: (Monad m) => In l sl r m a -> MachineReacT l sl r m a
basicFDE Nothing = do
  c' <- signal "Enter instruction"
  basicFDE c'
basicFDE (Just c) = do
  st <- lift $ get
  lift $ execute c
  c' <- signal "Enter next instruction"
  basicFDE c'

-- getIns :: IO (In l sl r m)
-- getIns = _

basicREPL :: (Integral r, Read l, Ord l)
          => InstructionSet l sl r m a
          -> Parser l
          -> (Machine l sl r -> IO ())
          -> Out
          -> MachineST l sl r IO (In l sl r m a)
basicREPL insset prs prnt out = do
  liftIO $ putStrLn out
  st <- get
  liftIO $ prnt st
  x <- liftIO $ parseRepl prs insset
  return x

-- startFDE :: (Monad m) => MachineReacT l sl r m a
-- startFDE = fde $ In Nothing Nothing

-- fde :: (Monad m) => Command ls sl r arg m a -> MachineReacT l sl r m a 
-- fde i = do
--   st <- lift get
--   lift $ processIncomingMemory i
--   m_ins <- case _instruction i of
--                 Just raw_ins -> do
--                   -- let ins =  decode raw_ins -- unsafe
--                   lift $ exec raw_ins
--                   return (Just raw_ins)
--                 Nothing      -> return Nothing
--   x ← lift $ makeOutbound m_ins
--   lift $ incrementPC
--   i' ← signal x
--   fde i'

 
