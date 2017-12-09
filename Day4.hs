import Data.List

noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = nub xs == xs

part1 :: FilePath -> IO Int
part1 = fmap (length . filter noDuplicates . fmap words . lines) . readFile

part2 :: FilePath -> IO Int
part2 = fmap (length . filter noDuplicates . fmap (fmap sort . words) . lines)
        . readFile
