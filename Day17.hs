import Data.List

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) . drop n . cycle $ xs

spinlock :: Int -> [a] -> a -> [a]
spinlock i xs j = cycles i xs ++ [j]

cumulate :: Traversable t => (b -> a -> b) -> b -> t a -> t b
cumulate f = fmap snd . mapAccumL (\acc x -> (f acc x, acc))

part1 :: Integer
part1 = head $ (cumulate (spinlock 303) [0] [1..]) !! 2017

after0 :: Integral a => a -> a -> a -> [a]
after0 l' l i | r == 0    = l  : after0 l  (l+1) (r + 1)
              | otherwise = l' : after0 l' (l+1) (r + 1)
  where r = (i + 303) `rem` l

part2 :: Integer
part2 = after0 1 1 0 !! 50000000
