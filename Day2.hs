checksum :: (Num a, Foldable t, Ord a) => [t a] -> a
checksum xss = sum [ maximum xs - minimum xs | xs <- xss ]

checksum2 :: Integral a => [[a]] -> a
checksum2 xss = sum [ x `div` y
                    | xs <- xss, x <- xs, y <- xs, x /= y, x `rem` y == 0 ]

ofSpreadSheet :: String -> [[Int]]
ofSpreadSheet = fmap (fmap read . words) . lines

part1 :: FilePath -> IO Int
part1 file = fmap (checksum . ofSpreadSheet) . readFile $ file

part2 :: FilePath -> IO Int
part2 file = fmap (checksum2 . ofSpreadSheet) . readFile $ file
