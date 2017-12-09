{-# LANGUAGE FlexibleContexts #-}
import Data.Array.IO
import Data.Array.Unboxed

next :: (MArray a i m, Ix i, Num i) => a i i -> (i -> i) -> i -> m i
next arr modify instr = do
  jmp <- readArray arr instr
  let instr' = instr + jmp
  writeArray arr instr (modify jmp)
  pure instr'

go :: (MArray a i m, Ix i, Num i, Show i) => a i i -> (i -> i) -> i -> i -> m i
go arr modify instr count = do
  (a,b) <- getBounds arr
  if instr < a || instr > b then pure count
    else do
    instr' <- next arr modify instr
    go arr modify instr' (count + 1)

escape :: (Int -> Int) -> FilePath -> IO Int
escape modify file = do
  instrs <- fmap (fmap read . lines) . readFile $ file
  let arr = listArray (1,length instrs) instrs :: UArray Int Int
  arr <- thaw arr :: IO (IOArray Int Int)
  go arr modify 1 0

part1 :: FilePath -> IO Int
part1 = escape (+1)

part2 :: FilePath -> IO Int
part2 = escape (\i -> if i >= 3 then i - 1 else i + 1)

main = part2 "/home/phil/Downloads/input" >>= print
