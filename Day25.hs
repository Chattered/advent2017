import qualified Data.Map.Strict as M

data Machine =
  Machine { tape :: M.Map Int ()
          , pos :: Int
          , state :: Label
          }

data Label = A | B | C | D | E | F

step :: Machine -> Machine
step machine =
  (case state machine of
     A -> view (\b -> (if b then set F else set B) . moveRight . switch)
     B -> view (\b -> (if b then set C else set B) . moveLeft)
     C -> view (\b -> (if b then set C . moveRight else set D . moveLeft)
                      . switch)
     D -> view (\b -> if b then set A . moveRight
                      else set E . moveLeft . switch)
     E -> view (\b -> (if b then set D else set F) . moveLeft . switch)
     F -> view (\b -> (if b then set E . moveLeft else set A . moveRight)
                      . switch))
  $! machine

set :: Label -> Machine -> Machine
set l m = m { state = l }

view :: (Bool -> Machine -> Machine) -> Machine -> Machine
view f m = f (pos m `M.member` tape m) m

moveLeft :: Machine -> Machine
moveLeft m = m { pos = pos m - 1 }

moveRight :: Machine -> Machine
moveRight m = m { pos = pos m + 1 }

switch :: Machine -> Machine
switch m = view (\b _ -> if b then m { tape = pos m `M.delete` tape m }
                         else m { tape = M.insert (pos m) () (tape m) }) m

part1 :: Int
part1 = fmap (M.size . tape) (iterate step (Machine M.empty 0 A)) !! 12425180
