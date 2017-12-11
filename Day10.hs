import Data.Bits
import Data.Char
import Numeric

skip :: Int -> [a] -> [a]
skip n xs = take (length xs) . drop n . cycle $ xs

flipSection :: Int -> [a] -> [a]
flipSection = flipS []
  where flipS acc 0 xs     = acc ++ xs
        flipS acc n (x:xs) = flipS (x:acc) (n-1) xs

knot :: Int -> [Int] -> [a] -> ([a],Int)
knot skipLength [] xs     = (xs,skipLength)
knot skipLength (l:ls) xs =
  knot (skipLength + 1) ls (skip (skipLength + l) (flipSection l xs))

knotRollback :: [Int] -> [a] -> [a]
knotRollback ls xs = skip (rollback `mod` (length xs)) . fst . knot 0 ls $ xs
  where rollback = negate (sum ls + (sum [0..length ls - 1]))

part1 :: Integer
part1 =
  product
  . take 2
  . knotRollback [18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188]
  $ [0..255]

sparseHash :: [Int] -> [a] -> [a]
sparseHash ls xs =
  skip (rollback `mod` (length xs)) . fst $ iterate f (xs,0) !! 64
  where rollback = negate (sum ls * 64 + (triangle (length ls * 64 - 1)))
        f (xs',skipLength) = knot skipLength ls xs'
        triangle n = n * (n + 1) `div` 2

takes :: Int -> [a] -> [[a]]
takes _ [] = []
takes n xs = let (ys,zs) = splitAt n xs in ys : takes n zs

denseHash :: (Num a, Bits a) => [a] -> [a]
denseHash = fmap (foldl xor 0) . takes 16

knotHash :: String -> String
knotHash str =
  let ls = fmap ord str ++ [17,31,73,47,23] in
  concatMap showHex2 . denseHash . sparseHash ls $ [(0::Int)..255]

part2 :: String
part2 = knotHash "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"

showHex2 :: (Integral a, Show a) => a -> String
showHex2 n = reverse (take 2 (reverse (showHex n "") ++ repeat '0'))
