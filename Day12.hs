{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Data.Attoparsec.Text hiding (takeWhile)
import Data.List hiding (takeWhile, union)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T

explore :: Ord a => M.Map a (S.Set a) -> a -> State (S.Set a) (S.Set a)
explore m x = do
  visited <- get
  let xs = fromMaybe S.empty (M.lookup x m) `S.difference` visited
  put (visited `S.union` xs)
  pure xs

unions :: Ord a => [S.Set a] -> S.Set a
unions = foldl S.union S.empty

exploreAll :: Ord a =>
              M.Map a (S.Set a) -> S.Set a -> State (S.Set a) [S.Set a]
exploreAll m xs =
   fmap (xs:) (exploreAll m
               =<< (fmap unions (traverse (explore m) . S.toList $ xs)))

orbit :: Ord a => M.Map a (S.Set a) -> a -> S.Set a
orbit m x = unions . takeWhile (not . S.null) . evalState (exploreAll m s) $ s
  where s = S.singleton x

nextOrbit :: Ord a => M.Map a (S.Set a) -> S.Set a -> Maybe (S.Set a,S.Set a)
nextOrbit m visited =
  fmap f . listToMaybe . S.toList $ (M.keysSet m `S.difference` visited)
  where f x = let os = orbit m x in (os, os `S.union` visited)

buildMap :: Ord a => (a,[a]) -> M.Map a (S.Set a)
buildMap (x,ys) =
  foldl (M.unionWith S.union) (M.singleton x (S.fromList ys))
  . fmap (`M.singleton` (S.singleton x)) $ ys

parsePipe :: Integral a => Parser (M.Map a (S.Set a))
parsePipe = do
  x <- fmap round scientific
  skipSpace
  string "<->"
  skipSpace
  ys <- sepBy (fmap round scientific) (string ", ")
  pure (buildMap (x,ys))

buildWorld :: Integral a => FilePath -> IO (M.Map a (S.Set a))
buildWorld filePath = do
  Right pipes <- fmap (sequence . fmap (parseOnly parsePipe) . T.lines . T.pack)
                 . readFile $ filePath
  pure (foldl (M.unionWith S.union) M.empty pipes)

part1 :: FilePath -> IO Int
part1 filePath = do
  world <- buildWorld filePath
  pure . S.size . orbit world $ 0

part2 :: FilePath -> IO Int
part2 filePath = do
  world <- buildWorld filePath
  pure (length . unfoldr (nextOrbit world) $ S.empty)
