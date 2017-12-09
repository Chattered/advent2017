{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Array.MArray
import qualified Data.Map as M

foldWithIndex :: (MArray a e m, Ix i, Enum i) =>
                 (b -> e -> i -> b) -> b -> a i e -> m b
foldWithIndex f b arr = do
  (l,_) <- getBounds arr
  arr' <- getElems arr
  pure . snd . foldl (\(i,b) x -> (succ i,f b x i)) (l,b) $ arr'

loopUntil :: (MArray a e m, Ix i, Enum i) =>
             (b -> e -> Maybe (b,e)) -> b -> i -> a i e -> m ()
loopUntil f b i arr = do
  (l,u) <- getBounds arr
  if i > u then loopUntil f b l arr else do
    x <- readArray arr i
    case f b x of
      Just (b',y) -> writeArray arr i y >> loopUntil f b' (succ i) arr
      Nothing -> pure ()

maxIndex :: (MArray a e m, Ix i, Enum i, Num e, Ord e) =>
            a i e -> m (Maybe (i, e))
maxIndex = foldWithIndex f Nothing
  where f Nothing x i = Just (i,x)
        f (Just (i,m)) x j = Just (if x > m then (j,x) else (i,m))

redistribute :: (MArray a e m, Num e, Ord e) => a Int e -> m ()
redistribute arr = do
  Just (i,x) <- maxIndex arr
  writeArray arr i 0
  loopUntil f x (succ i) arr
    where f 0 y   = Nothing
          f acc y = Just (acc-1, y + 1)

findCycle :: (MArray a e m, Num e, Ord e, Show e)
             => M.Map [e] Int -> Int -> a Int e -> m (Int, Int)
findCycle visited i arr = do
  xs <- getElems arr
  let visitedLength = M.lookup xs visited
  case visitedLength of
    Nothing -> redistribute arr >> findCycle (M.insert xs i visited) (i+1) arr
    Just j  -> pure (i,i - j)

part1 :: [Int] -> IO Int
part1 xs = fmap fst ((newListArray (0,length xs - 1) xs :: IO (IOArray Int Int))
                     >>= findCycle M.empty 0)

part2 :: [Int] -> IO Int
part2 xs = fmap snd ((newListArray (0,length xs - 1) xs :: IO (IOArray Int Int))
                     >>= findCycle M.empty 0)
