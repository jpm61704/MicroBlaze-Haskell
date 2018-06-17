module Boilerplate.Parse where

import Parsing
import Boilerplate.Instruction
import qualified Data.Map.Lazy as M
import qualified Text.Read as R


parseCommand :: (Integral r, Read l, Ord l) => Parser l -> InstructionSet l sl r m a -> Parser (Command l sl r m a)
parseCommand p insset = do
  ins <- parseInstruction insset
  args <- parseArgs p $ form ins
  return $ Command ins args


parseInstruction :: InstructionSet l sl r m a -> Parser (Instruction l sl r m a)
parseInstruction insset = do
  ins <- many1 alphanum
  case M.lookup ins insset of
    Just x -> return x
    Nothing -> fail "Not a valid instruction"

parseArgs :: (Integral r, Read l, Ord l) => (Parser l) -> ArgumentsForm l -> Parser (Arguments l r)
parseArgs _ (Form [])     = return (Args M.empty)
parseArgs p (Form (x:xs)) = do
  (k,v) <- parseArg p x
  Args args <- parseArgs p (Form xs)
  return $ Args $ M.insert k v args

parseArg :: (Integral r, Ord l) => Parser l -> (ArgumentForm,l) -> Parser (l, Argument l r )
parseArg p (form,l) = do
  arg <- case form of
    Reg -> parseRegister p
    Imm -> parseData
  return (l, arg)

parseRegister :: Parser l -> Parser (Argument l r)
parseRegister p = do
  reg <- p
  return $ Register reg

parseData :: (Integral r) => Parser (Argument l r)
parseData = do
  x <- integer
  return $ Immediate $ fromIntegral x

parseRepl :: (Integral r, Read l, Ord l)
          => Parser l
          -> InstructionSet l sl r m a
          -> IO (Maybe (Command l sl r m a))
parseRepl parse_reg insset = do
  s <- getLine
  let pr = parse (parseCommand parse_reg insset) s
  return $ case pr of
             [] -> Nothing
             [(ss,_)] -> Just ss
             _ -> Nothing
