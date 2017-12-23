{-# LANGUAGE OverloadedStrings #-}

import Day18 hiding (run1, part1, part2)

import Control.Applicative hiding (Const)
import Data.Attoparsec.Text
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- Hack
run1 :: V.Vector (Instr Char) -> Process Char ->
        Maybe (Bool, Process Char)
run1 instrs p | instrPtr p < 0 || instrPtr p >= length instrs = Nothing
run1 instrs p@(Process pid ptr regs buff sends) =
  Just $ case instrs V.! ptr of
  Snd x   -> (False, advance 1 . send (access x p) $ p)
  Set r x -> (False, advance 1 . set r x $ p)
  Add r x -> (False, runOp (flip (-)) r x p)
  Mul r x -> (True,  runOp (*) r x p)
  Mod r x -> (False, runOp (flip mod) r x p)
  Rcv r   -> (False, advance 1 p)
  Jgz c o -> (False, advance (if access c p /= 0 then access o p else 1) p)

parseInstr' :: Parser (Instr Char)
parseInstr' =
  (string "jnz " *> liftA2 Jgz (operand <* skipSpace) operand)
  <|> (string "sub " *> liftA2 Add (anyChar <* skipSpace) operand)
  <|> parseInstr
  where operand = fmap Const integer <|> fmap Reg anyChar
        integer = fmap round scientific

readInstrs' :: FilePath -> IO (Either String (V.Vector (Instr Char)))
readInstrs' = fmap (fmap V.fromList
                   . traverse (parseOnly parseInstr') . T.lines . T.pack)
             . readFile

composite :: Integral a => a -> Bool
composite n = any (\m -> n `rem` m == 0) [2..n-1]

composites :: Integral a => a -> a -> a -> Int
composites m n step = length (filter composite [m,(m+step)..n])

part1 :: FilePath -> IO Int
part1 filePath = do
  Right instrs <- readInstrs' filePath
  pure . length . filter id . unfoldr (run1 instrs)
    $ (Process 0 0 (M.empty) [] 0)

-- Is this cheating?
part2 :: Int
part2 = composites 106700 123700 17
