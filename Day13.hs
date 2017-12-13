import Control.Applicative
import Data.Attoparsec.Text hiding (scan)
import Data.Bifunctor
import Data.List (findIndex)
import qualified Data.Text as T

pos :: Int -> Int -> Int
pos span t =
  let p2 = t `rem` (span * 2 - 2) in
  if p2 >= span then 2 * span - 2 - p2 else p2

caughtAt :: (Int,Int) -> Bool
caughtAt (depth,span) = pos span depth == 0

severity :: [(Int, Int)] -> Int
severity = sum . fmap (uncurry (*)) . filter caughtAt

caughtsDelayed :: [(Int, Int)] -> [[(Int, Int)]]
caughtsDelayed firewall =
  [ filter caughtAt . fmap (first (+ i)) $ firewall
  | i <- [0..] ]

parseDepth :: Parser (Int,Int)
parseDepth = liftA2 (,)
             (fmap round scientific <* char ':' <* skipSpace)
             (fmap round scientific)

readFirewall :: FilePath -> IO (Either String [(Int, Int)])
readFirewall = fmap (sequence . fmap (parseOnly parseDepth) . T.lines . T.pack)
               . readFile

part1 :: FilePath -> IO (Either String Int)
part1 = (fmap.fmap) severity . readFirewall

part2 :: FilePath -> IO (Either String (Maybe Int))
part2 = (fmap.fmap) (findIndex null . caughtsDelayed) . readFirewall
