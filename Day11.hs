{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List
import Linear.Vector
import Linear.V2

ne = V2 1 0
n  = V2 0 1
nw = sw ^+^ n
sw = -1 *^ ne
s  = -1 *^ n
se = -1 *^ nw

hexDistance :: (Ord a, Num a) => V2 a -> a
hexDistance (V2 x y) =
  if x * y >= 0 then abs (x + y)
  else if x < 0 then
         if -x <= y then y else -x
       else if x <= -y then -y else x

cumulate :: Traversable t => (b -> a -> b) -> b -> t a -> (b, t b)
cumulate f = mapAccumL (\acc x -> (f acc x, acc))

path = cumulate (^+^) zero

pathEnd :: (Additive f, Num a) => [f a] -> f a
pathEnd = foldr (^+^) zero

parseDir :: Parser (V2 Integer)
parseDir = choice [string "ne" *> pure ne
                  ,string "nw" *> pure nw
                  ,string "sw" *> pure sw
                  ,string "se" *> pure se
                  ,string "n"  *> pure n
                  ,string "s"  *> pure s]

readInput :: FilePath -> IO (Either String [V2 Integer])
readInput = fmap (parseOnly (sepBy parseDir (string ","))) . BS.readFile

part1 :: FilePath -> IO (Either String Integer)
part1 = (fmap.fmap) (hexDistance . fst . path) . readInput

part2 :: FilePath -> IO (Either String Integer)
part2 = (fmap.fmap) (maximum . fmap hexDistance . snd . path)
        . readInput
