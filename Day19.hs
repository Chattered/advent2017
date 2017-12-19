import Data.Array
import Data.List
import Data.Maybe
import Linear hiding (transpose)
import Linear.V2

data Cursor = Cursor { pos :: V2 Int, dir :: V2 Int }

compass :: Num a => [V2 a]
compass = [ V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1) ]

run1 :: Array (V2 Int) Char -> Cursor -> Maybe (Maybe Char, Cursor)
run1 world c | not (inRange (bounds world) p') = Nothing
             | otherwise =
  case world ! p' of
  '+' -> Just (Nothing, turn world c')
  ' ' -> Nothing
  c | c >= 'A' && c <= 'Z' -> Just (Just c, c')
  _   -> Just (Nothing, c')
  where p' = pos c ^+^ (dir c)
        c' = Cursor p' (dir c)

turn :: Array (V2 Int) Char -> Cursor -> Cursor
turn world c = head [ Cursor (pos c) v
                    | v <- compass
                    , v /= (-1) *^ dir c
                    , let p' = pos c ^+^ v
                    , inRange (bounds world) p'
                    , world ! p' /= ' ' ]

worldArray :: [String] -> Array (V2 Int) Char
worldArray xss = listArray ((V2 1 1),(V2 x y)) (concat (transpose xss'))
  where y = length xss
        x = maximum (fmap length xss)
        xss' = fmap (take x . (++ repeat ' ')) xss

findStart :: String -> Maybe Cursor
findStart xs = fmap (\x -> Cursor (V2 (x+1) 1) (V2 0 1)) (findIndex (== '|') xs)

readWorld :: FilePath -> IO (Array (V2 Int) Char, Cursor)
readWorld filePath = do
  allLines <- lines <$> readFile filePath
  let Just c = findStart (head allLines)
  pure (worldArray allLines, c)

part1 :: FilePath -> IO [Char]
part1 = (fmap . fmap) (uncurry collect) readWorld
  where collect world = catMaybes . unfoldr (run1 world)

part2 :: FilePath -> IO Int
part2 = (fmap . fmap) (uncurry count) readWorld
  where count world = succ . length . unfoldr (run1 world)
