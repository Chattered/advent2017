import Data.Maybe
import Data.List
import qualified Data.Map as M

manhatten :: Integral a => a -> a
manhatten n = let (x,y) = coordinates n in abs x + abs y

coordinates :: Integral a => a -> (a, a)
coordinates n =
  let n' = n - 1 in
  let b = floor . sqrt . fromIntegral $ n' in
  let r = (b + 1) `div` 2 in
  let r' = r * 2 - 1 in
  let (x,y) = moveWaypoints2 (n - r'*r') (r,r) [(r,-r),(-r,-r),(-r,r),(r,r)] in
  (x,y)

moveWaypoint :: (Ord a, Num a) => a -> a -> a -> (a, a)
moveWaypoint n x x' | n < dist = (x + signum (x' - x) * n, 0)
                    | otherwise = (x', n - dist)
  where dist = abs (x' - x)

moveWaypoint2 :: (Ord a, Num a) => a -> (a, a) -> (a, a) -> ((a, a), a)
moveWaypoint2 n (startX,startY) (destX,destY) =
  let (x',n')  = moveWaypoint n startX destX in
  let (y',n'') = moveWaypoint n' startY destY in
  ((x',y'), n'')

moveWaypoints2 :: (Ord a, Num a) => a -> (a, a) -> [(a, a)] -> (a, a)
moveWaypoints2 n (startX,startY) [] = (startX,startY)
moveWaypoints2 n (startX,startY) ((nextX,nextY):ws) =
  let ((endX',endY'),n') = moveWaypoint2 n (startX,startY) (nextX,nextY) in
  if n' >= 0 then moveWaypoints2 n' (endX',endY') ws
  else (endX',endY')

spiralOut :: [Integer]
spiralOut = snd . mapAccumL f (M.singleton (0,0) 1) . map coordinates $ [2..]
  where f m (x,y) =
          let s = sum [ fromMaybe 0 (M.lookup (x+dx, y+dy) m)
                      | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0) ]
          in (M.insert (x,y) s m, s)

part1 :: Integral a => a -> a
part1 n = manhatten n

part2 :: Integer -> Integer
part2 n = head $ dropWhile (<= n) spiralOut
