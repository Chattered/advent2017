import Data.Bits

next :: Int -> Int -> Int
next m n = n * m `rem` 2147483647

genAs :: [Int]
genAs = iterate (next 16807) 634

genBs :: [Int]
genBs = iterate (next 48271) 301

countJudgements :: Int -> [Int] -> [Int] -> Int
countJudgements n as bs =
  length . filter id . take n
  $ zipWith (==) (fmap (.&. 0xffff) as) (fmap (.&. 0xffff) bs)

part1 :: Int
part1 = countJudgements 40000000 genAs genBs

part2 :: Int
part2 = countJudgements 5000000 as bs
   where as = filter (\n -> n .&. 3 == 0) genAs
         bs = filter (\n -> n .&. 7 == 0) genBs
