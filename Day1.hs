sumMatches :: (Num a, Eq a) => [(a,a)] -> a
sumMatches xs = sum [ x | (x,y) <- xs, x == y ]

pairUp :: Int -> [a] -> [(a, a)]
pairUp n xs = zip xs (drop n xs)

pairs :: Int -> [a] -> [(a, a)]
pairs n xs = take (length xs) (pairUp n (cycle xs))

parseDigits :: String -> [Int]
parseDigits = fmap (read . pure)

part1 :: String -> Int
part1 = sumMatches . pairs 1 . parseDigits

part2 :: String -> Int
part2 xs = sumMatches . pairs (length xs `div` 2) . parseDigits $ xs
