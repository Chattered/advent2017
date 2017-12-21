{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.Array
import Data.Foldable
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Linear.V2

flipX :: Num a => a -> V2 a -> V2 a
flipX n (V2 x y) = V2 x (n-y-1)

rot90 :: Num a => a -> V2 a -> V2 a
rot90 n (V2 x y) = V2 (n-y-1) x

transforms :: Num a => a -> [V2 a -> V2 a]
transforms n = direct ++ indirect
  where direct   = take 4 (iterate (rot90 n .) id)
        indirect = fmap (flipX n .) direct

makeShape :: (Integral i, Ix i) => [[e]] -> Array (V2 i) e
makeShape xss = listArray ((V2 0 0),(V2 (n-1) (n-1))) . concat . transpose $ xss
  where n = genericLength xss

transformShape :: Ix j => (j -> j) -> Array j e -> Array j e
transformShape t arr = ixmap (bounds arr) t arr

norm :: (Ord e, Ix t, Num t) => t -> Array (V2 t) e -> Array (V2 t) e
norm n arr = minimum (fmap (($ arr) . ixmap (bounds arr)) (transforms n))

splitsAt :: Integral a => a -> [b] -> [[b]]
splitsAt n [] = []
splitsAt n xs = let (ys,zs) = splitAt (fromIntegral n) xs in ys : splitsAt n zs

enhance :: (Ord e, Integral a, Integral i, Ix i, Ix a) =>
           ([e] -> [[f]]) -> Array (V2 a) e -> Array (V2 i) f
enhance enhancer arr | even size = uncurry merge (subnorm 2)
                     | otherwise = uncurry merge (subnorm 3)
  where (V2 lx ly, V2 mx my) = bounds arr
        splits n     = splitsAt n [lx..mx]
        subarr n vs  = listArray (V2 0 0,V2 (n-1) (n-1)) (fmap (arr !) vs)
        subnorm n    = (length $ splits n
                       , fmap (elems . norm n . subarr n)
                         $ liftA2 (liftA2 V2) (splits n) (splits n))
        merge m      = makeShape
                       . concatMap (fmap concat . transpose)
                       . transpose
                       . splitsAt m
                       . fmap enhancer
        size         = mx - lx + 1

parsePattern :: Parser (String, [String])
parsePattern = do
  lhs <- shape
  string " => "
  rhs <- shape
  pure (elems (norm (length lhs) (makeShape lhs)), rhs)
  where shape = sepBy (many (char '.' <|> char '#')) (char '/')

parseEnhancer :: FilePath -> IO (Either String (String -> [String]))
parseEnhancer = fmap (fmap ((M.!) . foldMap (uncurry M.singleton))
                      . traverse (parseOnly parsePattern)
                      . T.lines . T.pack)
                . readFile

readFractal :: (Integral a, Ix a) => FilePath -> IO [Array (V2 a) Char]
readFractal filePath = do
  Right enhancer <- parseEnhancer filePath
  pure (iterate (enhance enhancer) seedShape)

seedShape :: (Integral a, Ix a) => Array (V2 a) Char
seedShape = makeShape [".#.",
                       "..#",
                       "###"]

part1 :: FilePath -> IO Int
part1 = fmap (length . filter (== '#') . elems . (!! 5)) . readFractal

part2 :: FilePath -> IO Int
part2 = fmap (length . filter (== '#') . elems . (!! 18)) . readFractal
