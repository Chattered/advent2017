import Control.Applicative
import Data.Array
import Data.List (nub)
import Numeric
import Philed.Data.UnionFind
import Text.Printf

import Day10 hiding (part1,part2)

printBinary :: Int -> String
printBinary = printf "%.4b"

memory :: [[Bool]]
memory = [ fmap readBool . concat
           . fmap (printBinary . fst . head . readHex . pure)
           . knotHash $ "stpzcrnm-" ++ show i | i <- [0..127] ]

readBool :: Char -> Bool
readBool '0' = False
readBool '1' = True

part1 :: Int
part1 = length . filter id . concat $ memory

memoryArray :: (Ix i, Enum i) =>
               ((i, i),(i, i)) -> [[e]] -> Array (i,i) e
memoryArray ((tlx,tly),(brx,bry)) mem =
  array ((tlx,tly), (brx,bry))
  [ ((i,j),x)
  | (row,j) <- zip mem [tly..bry], (x,i) <- zip row [tlx..brx] ]

neighbours :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [(Int,Int)]
neighbours ((tlx,tly),(brx,bry)) (x,y) =
  filter inBounds [(x+1,y), (x,y+1) ]
  where inBounds (x,y) = x >= tlx && x <= brx && y >= tly && y <= bry

allPositions :: (Enum a, Enum b) => Array (a, b) e -> [(a, b)]
allPositions arr = liftA2 (,) [tlx..brx] [tly..bry]
  where ((tlx,tly),(brx,bry)) = bounds arr

regions :: Array (Int, Int) Bool
           -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
regions mem = runUF $ do
  sequence_ [ union (x,y) neighbour
            | (x,y) <- allPositions mem
            , neighbour <- neighbours (bounds mem) (x,y)
            , mem ! (x,y), mem ! neighbour ]
  sequence [ normal (x,y) | (x,y) <- allPositions mem, mem ! (x,y) ]

part2 :: Int
part2 = length . nub
        $ (regions (memoryArray ((0,0),(127,127)) memory)) ((0,0),(127,127))
