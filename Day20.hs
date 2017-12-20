{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Lens ((^.))
import Data.Attoparsec.Text
import Data.Function
import Data.List
import qualified Data.Text as T
import Linear
import Linear.V3

data Particle a = Particle { pos :: a , vel :: a , acc :: a }
                deriving (Eq,Functor)

instance Applicative Particle where
  pure x = Particle x x x
  Particle f g h <*> Particle x y z = Particle (f x) (g y) (h z)

instance Additive Particle where
  zero = Particle 0 0 0

parseParticle :: Parser (Particle (V3 Int))
parseParticle = do
  string "p=<"
  p <- v3
  string ">, v=<"
  v <- v3
  string ">, a=<"
  a <- v3
  pure (Particle p v a)
  where v3 = liftA3 V3 (integer <* char ',') (integer <* char ',') integer
        integer = fmap round scientific

readParticles :: FilePath -> IO (Either String [Particle (V3 Int)])
readParticles = fmap (traverse (parseOnly parseParticle) . T.lines . T.pack)
                . readFile

minimumsBy :: (a -> a -> Ordering) -> [a] -> [a]
minimumsBy cmp xs = foldl (\ms x -> add x (filter ((/= LT) . cmp x) ms)) [] xs
  where add x [] = [x]
        add x ys@(y:_) | cmp x y == GT = ys
                       | otherwise = x:ys

slowest :: [Particle (V3 Int)] -> [Integer]
slowest  = fmap fst
           . minimumsBy (p pos) . minimumsBy (p vel) . minimumsBy (p acc)
           . zip [0..]
  where p f = compare `on` (sum . fmap abs . f . snd)

part1 :: FilePath -> IO (Either String [Integer])
part1 = (fmap . fmap) slowest . readParticles

solveQuadratic :: (Ord a, Floating a) => a -> a -> a -> [a]
solveQuadratic 0 b c = [-c / b]
solveQuadratic a b c
  | d >= 0 = filter (>= 0)
             [((-b + sqrt d) / (2 * a)), ((-b - sqrt d) / (2 * a))]
  | otherwise = []
  where d = b*b - 4*a*c

solveParticle :: (Floating a, Ord a) => Particle a -> [a]
solveParticle (Particle p v a) = solveQuadratic (a / 2) (v + a / 2) p

solveParticle3 :: (Ord a, Floating a) => Particle (V3 a) -> [V3 a]
solveParticle3 p = liftA3 V3 (sp _x) (sp _y) (sp _z)
  where sp l = solveParticle (fmap (^. l) p)

collideAt :: (Integral a, Additive f, Applicative f, Eq (f a)) =>
             f a -> Particle (f a) -> Bool
collideAt t p = pos p ^+^ liftA2 (*) (vel p) t
                ^+^ fmap (`div` 2) (acc p `prod` (fmap pred t) `prod` t)
                == zero
  where prod = liftA2 (*)

candidates :: Integral a => [Particle (V3 a)] -> [Particle (V3 a)]
candidates [] = []
candidates (p:ps) =
  case span (collideEventually p) ps of
  ([],qs) -> candidates qs
  (ps',qs)  -> p:ps' ++ candidates qs

collideEventually :: Integral a => Particle (V3 a) -> Particle (V3 a) -> Bool
collideEventually p1 p2 =
  any (\t -> t ^. _x == t ^. _y && t ^. _x == t ^. _z && collideAt t p) ts
  where p = p2 ^-^ p1
        p' = (fmap . fmap) fromIntegral p :: Particle (V3 Double)
        ts = (fmap . fmap) round (solveParticle3 p')

runSim :: (Eq a, Ord a, Num a) => [Particle (V3 a)] -> [Particle (V3 a)]
runSim ps = filter (not . (`elem` collisions)) ps
  where collisions = concat . filter ((> 1) . length)
                     . groupBy (\p1 p2 -> pos p1 == pos p2)
                     . sortBy (compare `on` pos) $ ps

runToCollision :: (Eq a, Ord a, Num a) => [Particle (V3 a)] -> [Particle (V3 a)]
runToCollision = head . tail . nubBy (\xs ys -> length xs == length ys)
                 . iterate (fmap advance . runSim)

go :: Integral a => [Particle (V3 a)] -> Maybe (Int,[Particle (V3 a)])
go ps = go' (candidates ps)
  where go' [] = Nothing
        go' cs = Just (length cs - length remaining, remaining)
          where remaining = runToCollision cs

advance :: (Additive f, Num a) => Particle (f a) -> Particle (f a)
advance (Particle p v a) = Particle (p ^+^ v ^+^ a) (v ^+^ a) a

part2 :: FilePath -> IO (Either String Int)
part2 = (fmap . fmap) (\ps -> length ps - sum (unfoldr go ps)) . readParticles
