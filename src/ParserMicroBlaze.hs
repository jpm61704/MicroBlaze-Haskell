module ParserMicroBlaze where

import System.IO
import System.Environment
import Parsing
import InsSet
import Boilerplate
import qualified Boilerplate.W16 as W16

-- utility function to expand ~ in filenames
expandFilePath :: FilePath -> IO FilePath
expandFilePath ('~':'/':s) = do hd <- getEnv "HOME"
                                return (hd ++ "/" ++ s)
expandFilePath s           = return s

-- main entry points for parser
--parseMB :: FilePath -> IO [Command]
parseMB p_ = do p      <- expandFilePath p_
                s      <- readFile p
                let pr = parse (many1 parseCommand) s
                case pr of
                   [] -> fail "Argghhh!"
    (require 'haskell-prettify "~/.emacs.d/lisp/prettify-alists/haskell-prettify.el" t)
(add-hook 'haskell-mode-hook 'haskell-prettify-enable)
(add-hook 'haskell-interactive-mode-hook 'haskell-prettify-enable)               [(ss,_)] -> return ss
                   _         -> fail "Too many parses"

parseREPL :: IO [Command]
parseREPL = do
  s <- getLine
  let pr = parse (many1 parseCommand) s
  case pr of
    [] -> fail "arrrgh!"
    [(ss,_)] -> return ss
    _        -> fail "Too many parses"
  

data Op    = Plus | Minus | Times | Div deriving Show
data Exp   = Const Int | Aexp Op Exp Exp deriving Show
data Oper = Register Int deriving Show
data Imm = Immed Int deriving Show

type Command = Ins

parseOp = do
   isym <- (symbol "+") +++ (symbol "-") +++ (symbol "*") +++ (symbol "/")
   return (tr isym)
      where
         tr "+" = Plus
         tr "-" = Minus
         tr "*" = Times
         tr "/" = Div

parseConst = do
   i <- integer
   return (Const i)

parseAexp = do
   symbol "("
   op <- parseOp
   space
   e1 <- parseExp
   space
   e2 <- parseExp
   symbol ")"
   return (Aexp op e1 e2)

parseReg :: Parser MBReg
parseReg = do
   char 'r'
   n <- natural
   case numToReg n of
     Just reg -> return reg
     Nothing -> undefined
  

parseSpecReg :: Parser MBSReg
parseSpecReg = undefined


  
numToReg :: Int -> Maybe MBReg
numToReg x 
  | x < 0 || x > 32 = Nothing
  | otherwise = Just $ case x of
               0 -> R0
               1 -> R1
               2 -> R2
               3 -> R3
               4 -> R4
               5 -> R5
               6 -> R6
               7 -> R7
               8 -> R8
               9 -> R9
               10 -> R10
               11 -> R11
               12 -> R12
               13 -> R13
               14 -> R14
               15 -> R15
               16 -> R16
               17 -> R17
               18 -> R18
               19 -> R19
               20 -> R20
               21 -> R21
               22 -> R22
               23 -> R23
               24 -> R24
               25 -> R25
               26 -> R26
               27 -> R27
               28 -> R28
               29 -> R29
               30 -> R30
               31 -> R31
    
               


parseImm :: Parser W16
parseImm = do
   n <- natural   
   return $ W16.fromInteger n

parseExp = parseConst +++ parseAexp

parseCommand :: Parser Command
parseCommand =       parseAdd        +++ 
                     parseAddc       +++
                     parseAddk       +++
                     parseAddkc      +++
                     parseAddi       +++
                     parseAddic      +++
                     parseAddik      +++
                     parseAddikc     +++
                     parseAnd        +++
                     parseAndi       +++
                     parseAndn       +++
                     parseAndni      +++
                     parseBsrl       +++
                     parseBsra       +++
                     parseBsll       +++
                     parseBsrli      +++
                     parseBsrai      +++
                     parseBslli      +++
                     parseCmp        +++
                     parseCmpu       +++
                     parseIdiv       +++
                     parseIdivu      +++
                     parseLbu        +++
                     parseLbui       +++
                     parseLhu        +++
                     parseLhui       +++
                     parseLw         +++
                     parseLwi        +++
                     parseMul        +++
                     parseMuli       +++
                     parseOr         +++
                     parseOri        +++
                     parseRsub       +++
                     parseRsubc      +++
                     parseRsubk      +++
                     parseRsubkc     +++
                     parseRsubi      +++
                     parseRsubic     +++
                     parseRsubik     +++
                     parseRsubikc    +++
                     parseSb         +++
                     parseSbi        +++
                     parseSh         +++
                     parseShi        +++
                     parseSw         +++
                     parseSwi        +++
                     parseXor        +++
                     parseXori       +++
                     parseBeq        +++
                     parseBeqd       +++
                     parseBeqi       +++
                     parseBeqid      +++
                     parseBge        +++
                     parseBged       +++
                     parseBgei       +++
                     parseBgeid      +++
                     parseBgt        +++
                     parseBgtd       +++
                     parseBgti       +++
                     parseBgtid      +++
                     parseBle        +++
                     parseBled       +++
                     parseBlei       +++
                     parseBleid      +++
                     parseBlt        +++
                     parseBltd       +++
                     parseBlti       +++
                     parseBltid      +++
                     parseBne        +++
                     parseBned       +++
                     parseBnei       +++
                     parseBneid      +++
                     parseBrld       +++
                     parseBrald      +++
                     parseBrlid      +++
                     parseBralid     +++
                     parseBrk        +++
                     parseBrki       +++
                     parseMfs        +++
                     parseMts        +++
                     parseRtbd       +++
                     parseRtid       +++
                     parseRtsd       +++
                     parseSextHex    +++
                     parseSextOct    +++
                     parseSra        +++
                     parseSrc        +++
                     parseSrl        +++
                     parseWic        +++
                     parseBr         +++     
                     parseBra        +++
                     parseBrad       +++
                     parseBri        +++
                     parseBrai       +++
                     parseBrid       +++
                     parseBraid  --    +++
--                     parseIm         +++
  --                   parseLabel      


parseAdd :: Parser Command
parseAdd = do
   symbol "add"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Add r1 r2 r3)

parseAddc :: Parser Command
parseAddc = do
   symbol "addc"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Addc r1 r2 r3)

parseAddk :: Parser Command
parseAddk = do
   symbol "addk"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Addk r1 r2 r3)

parseAddkc :: Parser Command
parseAddkc = do
   symbol "addkc"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Addkc r1 r2 r3)

parseAddi :: Parser Command
parseAddi = do
   symbol "addi"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Addi r1 r2 imm3)

parseAddic :: Parser Command
parseAddic = do
   symbol "addic"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Addic r1 r2 imm3)

parseAddik :: Parser Command
parseAddik = do
   symbol "addik"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Addik r1 r2 imm3)

parseAddikc :: Parser Command
parseAddikc = do
    symbol "addikc"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Addikc r1 r2 imm3)

parseAnd :: Parser Command
parseAnd = do
    symbol "and"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    r3 <- parseReg
    return (And r1 r2 r3)

parseAndi :: Parser Command
parseAndi = do
    symbol "andi"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Addi r1 r2 imm3)

parseAndn :: Parser Command
parseAndn = do
    symbol "andn"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    r3 <- parseReg
    return (Andn r1 r2 r3)

parseAndni :: Parser Command
parseAndni = do
    symbol "andni"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Andni r1 r2 imm3)

parseBsrl :: Parser Command
parseBsrl = do
    symbol "bsrl"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    r3 <- parseReg
    return (Bsrl r1 r2 r3)

parseBsra :: Parser Command
parseBsra = do
    symbol "bsra"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    r3 <- parseReg
    return (Bsra r1 r2 r3)

parseBsll :: Parser Command
parseBsll = do
  symbol "bsll"
  r1 <- parseReg
  symbol ","
  r2 <- parseReg
  symbol ","
  r3 <- parseReg
  return (Bsll r1 r2 r3)

parseBsrli :: Parser Command
parseBsrli = do
  symbol "bsrli"
  r1 <- parseReg
  symbol ","
  r2 <- parseReg
  symbol ","
  imm3 <- parseImm
  return (Bsrli r1 r2 imm3)
         
parseBsrai :: Parser Command
parseBsrai = do
    symbol "bsrai"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Bsrai r1 r2 imm3)

parseBslli :: Parser Command
parseBslli = do
    symbol "bslli"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Bslli r1 r2 imm3)

parseCmp :: Parser Command
parseCmp = do
   symbol "cmp"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Cmp r1 r2 r3)
         
parseCmpu :: Parser Command
parseCmpu = do
   symbol "cmpu"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Cmpu r1 r2 r3)

parseIdiv :: Parser Command
parseIdiv = do
   symbol "idiv"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Idiv r1 r2 r3)

parseIdivu :: Parser Command
parseIdivu = do
   symbol "idivu"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Idivu r1 r2 r3)
         

parseLbu :: Parser Command
parseLbu = do
   symbol "lbu"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Lbu r1 r2 r3)

parseLbui :: Parser Command
parseLbui = do
    symbol "lbui"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Lbui r1 r2 imm3)

parseLhu :: Parser Command
parseLhu = do
   symbol "lhu"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Lhu r1 r2 r3)

parseLhui :: Parser Command
parseLhui = do
    symbol "lhui"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Lhui r1 r2 imm3)
         
parseLw :: Parser Command
parseLw = do
   symbol "lw"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Lw r1 r2 r3)

parseLwi :: Parser Command
parseLwi = do
    symbol "lwi"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Lwi r1 r2 imm3)

parseMul :: Parser Command
parseMul = do
   symbol "mul"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Mul r1 r2 r3)

parseMuli :: Parser Command
parseMuli = do
    symbol "muli"
    r1 <- parseReg
    symbol ","
    r2 <- parseReg
    symbol ","
    imm3 <- parseImm
    return (Muli r1 r2 imm3)

parseOr :: Parser Command
parseOr = do
   symbol "or"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Or r1 r2 r3)

parseOri :: Parser Command
parseOri = do
   symbol "ori"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Ori r1 r2 imm3)

parseRsub :: Parser Command
parseRsub = do
   symbol "rsub"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Rsub r1 r2 r3)

parseRsubc :: Parser Command
parseRsubc = do
   symbol "rsubc"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Rsubc r1 r2 r3)

parseRsubk :: Parser Command
parseRsubk = do
   symbol "rsubk"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Rsubk r1 r2 r3)

parseRsubkc :: Parser Command
parseRsubkc = do
   symbol "rsubkc"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Rsubkc r1 r2 r3)

parseRsubi :: Parser Command
parseRsubi = do
   symbol "rsubi"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Rsubi r1 r2 imm3)

parseRsubic :: Parser Command
parseRsubic = do
   symbol "rsubic"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Rsubic r1 r2 imm3)

parseRsubik :: Parser Command
parseRsubik = do
   symbol "rsubik"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Rsubik r1 r2 imm3)

parseRsubikc :: Parser Command
parseRsubikc = do
   symbol "rsubikc"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Rsubikc r1 r2 imm3)

parseSb :: Parser Command
parseSb = do
   symbol "sb"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Sb r1 r2 r3)

parseSbi :: Parser Command
parseSbi = do
   symbol "sbi"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Sbi r1 r2 imm3)

parseSh :: Parser Command
parseSh = do
   symbol "sb"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Sb r1 r2 r3)


parseShi :: Parser Command
parseShi = do
   symbol "shi"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Shi r1 r2 imm3)


parseSw :: Parser Command
parseSw = do
   symbol "sw"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Sw r1 r2 r3)


parseSwi :: Parser Command
parseSwi = do
   symbol "shi"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Swi r1 r2 imm3)
    
parseXor :: Parser Command
parseXor = do
   symbol "xor"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   r3 <- parseReg
   return (Xor r1 r2 r3)


parseXori :: Parser Command
parseXori = do
   symbol "xori"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   symbol ","
   imm3 <- parseImm
   return (Xori r1 r2 imm3)

parseBeq :: Parser Command
parseBeq = do
   symbol "beq"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Beq r1 r2 w11_blank)

parseBeqd :: Parser Command
parseBeqd = do
   symbol "beqd"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Beqd r1 r2 w11_blank)

    
parseBeqi :: Parser Command
parseBeqi = do
   symbol "beqi"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Beqi r1 imm2)

parseBeqid :: Parser Command
parseBeqid = do
   symbol "beqid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Beqid r1 imm2)

parseBge :: Parser Command
parseBge = do
   symbol "bge"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bge r1 r2 w11_blank)

parseBged :: Parser Command
parseBged = do
   symbol "bged"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bged r1 r2 w11_blank)

    
parseBgei :: Parser Command
parseBgei = do
   symbol "bgei"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bgei r1 imm2)

parseBgeid :: Parser Command
parseBgeid = do
   symbol "bgeid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bgeid r1 imm2)

parseBgt :: Parser Command
parseBgt = do
   symbol "bgt"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bgt r1 r2 w11_blank)

parseBgtd :: Parser Command
parseBgtd = do
   symbol "bgtd"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bgtd r1 r2 w11_blank)

    
parseBgti :: Parser Command
parseBgti = do
   symbol "bgti"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bgti r1 imm2)

parseBgtid :: Parser Command
parseBgtid = do
   symbol "bgtid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bgtid r1 imm2)

parseBle :: Parser Command
parseBle = do
   symbol "ble"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Ble r1 r2 w11_blank)

parseBled :: Parser Command
parseBled = do
   symbol "bled"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bled r1 r2 w11_blank)

    
parseBlei :: Parser Command
parseBlei = do
   symbol "blei"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Blei r1 imm2)

parseBleid :: Parser Command
parseBleid = do
   symbol "bleid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bleid r1 imm2)


parseBlt :: Parser Command
parseBlt = do
   symbol "blt"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Blt r1 r2 w11_blank)

parseBltd :: Parser Command
parseBltd = do
   symbol "bltd"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bltd r1 r2 w11_blank)

    
parseBlti :: Parser Command
parseBlti = do
   symbol "blti"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Blti r1 imm2)

parseBltid :: Parser Command
parseBltid = do
   symbol "bltid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bltid r1 imm2)

    
parseBne :: Parser Command
parseBne = do
   symbol "bne"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bne r1 r2 w11_blank)

parseBned :: Parser Command
parseBned = do
   symbol "bned"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Bned r1 r2 w11_blank)

    
parseBnei :: Parser Command
parseBnei = do
   symbol "bnei"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bnei r1 imm2)

parseBneid :: Parser Command
parseBneid = do
   symbol "bneid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bneid r1 imm2)

parseBrld :: Parser Command
parseBrld = do
   symbol "brld"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Brld r1 r2)

parseBrald :: Parser Command
parseBrald = do
   symbol "brald"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Brald r1 r2)

    
parseBrlid :: Parser Command
parseBrlid = do
   symbol "brlid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Brlid r1 imm2)

parseBralid :: Parser Command
parseBralid = do
   symbol "bralid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Bralid r1 imm2)

    
parseBrk :: Parser Command
parseBrk = do
   symbol "brk"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Brk r1 r2)
    
parseBrki :: Parser Command
parseBrki = do
   symbol "brki"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Brki r1 imm2)
    
parseMfs :: Parser Command
parseMfs = do
   symbol "mfs"
   r1 <- parseReg
   symbol ","
   r2 <- parseSpecReg
   return (Mfs r1 r2)
    
parseMts :: Parser Command
parseMts = do
   symbol "mts"
   r1 <- parseSpecReg
   symbol ","
   r2 <- parseReg
   return (Mts r1 r2)

    
parseRtbd :: Parser Command
parseRtbd = do
   symbol "rtbd"
   r1 <- parseReg
   symbol ","
   imm <- parseImm
   return (Rtbd r1 imm)
    
parseRtid :: Parser Command
parseRtid = do
   symbol "rtid"
   r1 <- parseReg
   symbol ","
   imm2 <- parseImm
   return (Rtid r1 imm2)

parseRtsd :: Parser Command
parseRtsd = do
   symbol "rtsd"
   r1 <- parseReg
   symbol ","
   imm <- parseImm
   return (Rtsd r1 imm)

parseSextHex :: Parser Command
parseSextHex = do
   symbol "sext16"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Sext16 r1 r2)
    
parseSextOct :: Parser Command
parseSextOct = do
   symbol "sext8"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Sext8 r1 r2)

parseSra :: Parser Command
parseSra = do
   symbol "sra"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Sra r1 r2)
    
parseSrc :: Parser Command
parseSrc = do
   symbol "src"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Src r1 r2)
    
parseSrl :: Parser Command
parseSrl = do
   symbol "srl"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Srl r1 r2)

parseWic :: Parser Command
parseWic = do
   symbol "wic"
   r1 <- parseReg
   symbol ","
   r2 <- parseReg
   return (Wic r1 r2)

parseBr :: Parser Command
parseBr = do
   symbol "br"
   r1 <- parseReg
   return (Br r1)

parseBra :: Parser Command
parseBra = do
   symbol "bra"
   r1 <- parseReg
   return (Bra r1)

parseBrad :: Parser Command
parseBrad = do
   symbol "brad"
   r1 <- parseReg
   return (Brad r1)

parseBri :: Parser Command
parseBri = do
   symbol "bri"
   imm1 <- parseImm
   return (Bri imm1)
    
parseBrai :: Parser Command
parseBrai = do
   symbol "brai"
   imm1 <- parseImm
   return (Brai imm1)
    
parseBrid :: Parser Command
parseBrid = do
   symbol "brid"
   imm1 <- parseImm
   return (Brid imm1)

parseBraid :: Parser Command
parseBraid = do
   symbol "braid"
   imm1 <- parseImm
   return (Braid imm1)


w11_blank = W11 C C C C C C C C C C C


{-
parseIm :: Parser Command
parseIm = do
  symbol "imm"
  imm1 <- parseImm
  return (Im imm1)

parseLabel :: Parser Command
parseLabel = do 
        symbol "label:"
        imm1 <- parseImm
        return (Label imm1)
-}
{-
parseNop :: Parser Command
parseNop = do
  symbol "nop"
  return (Nop)
-}
{-
Command =  Add Oper Oper Oper        |
                Addc Oper Oper Oper       |
                Addk Oper Oper Oper         |
                Addkc Oper Oper Oper      |
                Addi Oper Oper Imm   |
                Addic Oper Oper Imm                     |
                Addik Oper Oper Imm                        |
                Addikc Oper Oper Imm            |
                And Oper Oper Oper   |
                Andi Oper Oper Imm   |
                Andn Oper Oper Oper                        |
                Andni Oper Oper Imm                        |
                Bsrl Oper Oper Oper                        |
                Bsra Oper Oper Oper                        |
                Bsll Oper Oper Oper                        |
                Bsrli Oper Oper Imm                        |
                Bsrai Oper Oper Imm                        |
                Bslli Oper Oper Imm                        |
                Cmp Oper Oper Oper   |
                Cmpu Oper Oper Oper                        |
                Idiv Oper Oper Oper                        |
                Idivu Oper Oper Oper            |
                Lbu Oper Oper Oper   |
                Lbui Oper Oper Imm   |
                Lhu Oper Oper Oper   |
                Lhui Oper Oper Imm   |
                Lw Oper Oper Oper    |
                Lwi Oper Oper Imm    |
                Mul Oper Oper Oper   |
                Muli Oper Oper Imm   |
                Or Oper Oper Oper    |
                Ori Oper Oper Imm    |
                Rsub Oper Oper Oper                        |
                Rsubc Oper Oper Oper            |
                Rsubk Oper Oper Oper            |
                Rsubkc Oper Oper Oper           |
                Rsubi Oper Oper Imm                        |
                Rsubic Oper Oper Imm            |
                Rsubik Oper Oper Imm            |
                Rsubikc Oper Oper Imm           |
                Sb Oper Oper Oper    |
                Sbi Oper Oper Imm    |
                Sh Oper Oper Oper    |
                Shi Oper Oper Imm    |
                Sw Oper Oper Oper    |
                Swi Oper Oper Imm    |
                Xor Oper Oper Oper   |
                Xori Oper Oper Imm   |
                Beq Oper Oper        |
                Beqd Oper Oper       |
                Beqi Oper Imm        |
                Beqid Oper Imm       |
                Bge Oper Oper        |
                Bged Oper Oper       |
                Bgei Oper Imm        |
                Bgeid Oper Imm       |
                Bgt Oper Oper        |
                Bgtd Oper Oper       |
                Bgti Oper Imm        |
                Bgtid Oper Imm       |
                Ble Oper Oper        |
                Bled Oper Oper       |
                Blei Oper Imm        |
                Bleid Oper Imm       |
                Blt Oper Oper        |
                Bltd Oper Oper       |
                Blti Oper Imm        |
                Bltid Oper Imm       |
                Bne Oper Oper        |
                Bned Oper Oper       |
                Bnei Oper Imm        |
                Bneid Oper Imm       |
                Brld Oper Oper       |
                Brald Oper Oper      |
                Brlid Oper Imm       |
                Bralid Oper Imm      |
                Brk Oper Oper        |
                Brki Oper Imm        |
                Mfs Oper Oper        |
                Mts Oper Oper        |
                Rtbd Oper Oper       |
                Rtid Oper Imm        |
                Rtsd Oper Oper       |
                SextHex Oper Oper    |
                SextOct Oper Oper    |
                Sra Oper Oper        |
                Src Oper Oper        |
                Srl Oper Oper        |
                Wic Oper Oper        |
                Br Oper                         |
                Bra Oper                        |
                Brd Oper                        |
                Brad Oper                       |
                Bri Imm                         |
                Brai Imm                        |
                Brid Imm                        |
                Braid Imm                       |
                Im Imm                          |
                Label Imm                       |
                Nop
             deriving Show
-}
