import Data.List
import qualified Data.Map as M
import Data.Maybe
import Linear
import Linear.V2

data Cursor a = Cursor { pos :: V2 a , dir :: V2 a }

rot90 :: Num a => V2 a -> V2 a
rot90 (V2 x y) = V2 (-y) x

rot180 :: Num a => V2 a -> V2 a
rot180 (V2 x y) = V2 (-x) (-y)

rot270 :: Num a => V2 a -> V2 a
rot270 (V2 x y) = V2 y (-x)

type State a s = (Cursor a, M.Map (V2 a) s)

move :: Integral a => (Maybe s -> V2 a -> V2 a)
                      -> (Maybe s -> Maybe s)
                      -> State a s -> (Maybe s, State a s)
move turn change (c,w) = (change state, (Cursor (pos c ^+^ d') d', w'))
  where d'          = turn state (dir c)
        w'          = M.alter change (pos c) w
        state       = M.lookup (pos c) w

go1 :: Integral a => M.Map (V2 a) () -> [Bool]
go1 w = fmap isJust (unfoldr (Just . move f g) (Cursor (V2 0 0) (V2 0 1), w))
  where f x = if isJust x then rot270 else rot90
        g Nothing   = Just ()
        g (Just ()) = Nothing

data Evolved = Weakened | Infected | Flagged

nextEvolved :: Maybe Evolved -> Maybe Evolved
nextEvolved Nothing         = Just Weakened
nextEvolved (Just Weakened) = Just Infected
nextEvolved (Just Infected) = Just Flagged
nextEvolved (Just Flagged)  = Nothing

turnEvolved :: Integral a => Maybe Evolved -> V2 a -> V2 a
turnEvolved Nothing         = rot90
turnEvolved (Just Weakened) = id
turnEvolved (Just Infected) = rot270
turnEvolved (Just Flagged)  = rot180

isInfected :: Maybe Evolved -> Bool
isInfected (Just Infected) = True
isInfected _ = False

go2 :: Integral a => M.Map (V2 a) Evolved -> [Bool]
go2 w = fmap isInfected (unfoldr (Just . mv) (Cursor (V2 0 0) (V2 0 1), w))
  where f x = if isJust x then rot270 else rot90
        g Nothing   = Just ()
        g (Just ()) = Nothing
        mv = move turnEvolved nextEvolved

readWorld :: String -> M.Map (V2 Int) ()
readWorld str =
  M.unions [ M.singleton (V2 x y) () | (row,y) <- zip rows [my,my-1..(-my)]
                                     , (c,x)   <- zip row [-mx..mx]
                                     , c == '#' ]
  where rows = lines str
        nbColumns = length (head rows)
        nbRows = length rows
        mx = nbRows `div` 2
        my = nbColumns `div` 2

part1 :: FilePath -> IO Int
part1 = fmap (length . filter id . take 10000 . go1 . readWorld) . readFile

part2 :: FilePath -> IO Int
part2 = fmap (length . filter id . take 10000000
              . go2 . M.map (const Infected) . readWorld)
        . readFile
