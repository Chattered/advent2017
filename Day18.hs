{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative hiding (Const)
import Data.Attoparsec.Text
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

data Rand a = Const Int | Reg a

data Instr a = Snd (Rand a)
             | Set a (Rand a)
             | Add a (Rand a)
             | Mul a (Rand a)
             | Mod a (Rand a)
             | Rcv a
             | Jgz (Rand a) (Rand a)

data Process a =
  Process {
    pid :: Int,
    instrPtr :: Int,
    registers :: M.Map a Int,
    sendBuffer :: [Int],
    sends :: Int
  }

access :: Ord a => Rand a -> Process a -> Int
access (Const x) _ = x
access (Reg r)   p = M.findWithDefault 0 r (registers p)

get :: Ord a => a -> Process a -> Int
get r p = M.findWithDefault 0 r (registers p)

set :: Ord a => a -> Rand a -> Process a -> Process a
set r x p = p { registers = M.insert r (access x p) (registers p) }

advance :: Int -> Process a -> Process a
advance o p = p { instrPtr = instrPtr p + o }

send :: Int -> Process a -> Process a
send x p = p { sendBuffer = sendBuffer p ++ [x], sends = sends p + 1 }

consume :: Process a -> Process a
consume p = p { sendBuffer = drop 1 (sendBuffer p) }

parseInstr :: Parser (Instr Char)
parseInstr =
  (string "snd " *> fmap Snd operand)
  <|> (string "set " *> liftA2 Set (anyChar <* skipSpace) operand)
  <|> (string "add " *> liftA2 Add (anyChar <* skipSpace) operand)
  <|> (string "mul " *> liftA2 Mul (anyChar <* skipSpace) operand)
  <|> (string "mod " *> liftA2 Mod (anyChar <* skipSpace) operand)
  <|> (string "rcv " *> fmap Rcv anyChar)
  <|> (string "jgz " *> liftA2 Jgz (operand <* skipSpace) operand)
  where operand = fmap Const integer <|> fmap Reg anyChar
        integer = fmap round scientific

data ProcessState a = Ready | Blocked a | Terminated

runOp :: Ord a
         => (Int -> Int -> Int) -> a -> Rand a
         -> Process a -> Process a
runOp f r x p = p { instrPtr = succ (instrPtr p),
                    registers = M.adjust (f (access x p)) r (registers p) }

run1 :: Ord a => V.Vector (Instr a) -> Process a -> (ProcessState a, Process a)
run1 instrs p | instrPtr p < 0 || instrPtr p >= length instrs = (Terminated, p)
run1 instrs p@(Process pid ptr regs buff sends) =
  case instrs V.! ptr of
  Snd x   -> (Ready, advance 1 . send (access x p) $ p)
  Set r x -> (Ready, advance 1 . set r x $ p)
  Add r x -> (Ready, runOp (+) r x p)
  Mul r x -> (Ready, runOp (*) r x p)
  Mod r x -> (Ready, runOp (flip mod) r x p)
  Rcv r   -> (Blocked r, advance 1 p)
  Jgz c o -> (Ready, advance (if access c p > 0 then access o p else 1) p)

lastSound :: Ord a => (ProcessState a, Process a) -> V.Vector (Instr a) -> Int
lastSound (Ready, p) instrs = lastSound (run1 instrs p) instrs
lastSound (Blocked r, p) instrs
  | get r p == 0 = lastSound (run1 instrs (advance 1 p)) instrs
  | otherwise = last (sendBuffer p)

dual :: Ord a =>
        V.Vector (Instr a)
        -> (ProcessState a, Process a)
        -> (ProcessState a, Process a)
        -> (Process a, Process a)
dual instrs (Terminated, p1) (_,p2) = (p1,p2)
dual instrs (_,p1) (Terminated,p2)  = (p1,p2)
dual instrs (Ready, p1) p2          = dual instrs (run1 instrs p1) p2
dual instrs ps1 ps2@(Ready, p2)     = dual instrs ps2 ps1

dual instrs ps1@(Blocked r, p1) ps2@(Blocked r',p2) =
  case (sendBuffer p1, sendBuffer p2) of
  ([],[])   -> (p1,p2)
  (_,s:ss) -> dual instrs (Ready, set r (Const s) p1)  (Blocked r', consume p2)
  (s:ss,_) -> dual instrs (Ready, set r' (Const s) p2) (Blocked r, consume p1)

initProcess :: Int -> (ProcessState Char, Process Char)
initProcess pid = (Ready, Process pid 0 (M.singleton 'p' pid) [] 0)

readInstrs :: FilePath -> IO (Either String (V.Vector (Instr Char)))
readInstrs = fmap (fmap V.fromList
                   . traverse (parseOnly parseInstr) . T.lines . T.pack)
             . readFile

part1 :: FilePath -> IO (Either String Int)
part1 = (fmap . fmap) (lastSound (initProcess 0)) . readInstrs

part2 :: FilePath -> IO Int
part2 filePath = do
  Right instrs <- readInstrs filePath
  let (p1,p2) = dual instrs (initProcess 0) (initProcess 1)
  pure (sends (sortBy (compare `on` pid) [p1,p2] !! 1))
