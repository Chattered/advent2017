import Data.Bifunctor
import Data.Function
import Data.List

selectBy :: (a -> Bool) -> [a] -> [(a, [a])]
selectBy p [] = []
selectBy p (x:xs) | p x = (x,xs) : ys
                  | otherwise = ys
  where ys = (fmap . fmap) (x:) (selectBy p xs)

other :: Eq a => (a,a) -> a -> a
other (x,y) z | x == z = y
              | otherwise = x

bridges :: Ord a => a -> [(a,a)] -> [[(a,a)]]
bridges input ports =
  case selectBy (\(x,y) -> x == input || y == input) ports of
  [] -> [[]]
  foo -> [ (x,y):b
         | ((x,y),ports') <- foo
         , let output = other (x,y) input
         , b <- bridges output ports' ]

maximumsBy :: (a -> a -> Ordering) -> [a] -> [a]
maximumsBy p xs = let m = maximumBy p xs in filter (\x -> p m x == EQ) xs

parsePipe :: String -> (Int, Int)
parsePipe = bimap read (read . tail) . span (/= '/')

readPipes :: FilePath -> IO [(Int, Int)]
readPipes = fmap (fmap parsePipe . lines) . readFile

strength :: Num a => [(a,a)] -> a
strength = sum . fmap (uncurry (+))

part1 :: FilePath -> IO Int
part1 = fmap (strength . maximumBy (compare `on` strength) . bridges 0)
        . readPipes

part2 :: FilePath -> IO Int
part2 = fmap (strength . maximumBy (compare `on` length)
              . maximumsBy (compare `on` length) . bridges 0)
        . readPipes
