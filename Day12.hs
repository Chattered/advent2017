{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Data.Attoparsec.Text hiding (takeWhile)
import Data.List hiding (takeWhile)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T

import Debug.Trace

explore :: Ord a => M.Map a [a] -> a -> State (S.Set a) [a]
explore m x = do
  visited <- get
  let xs = filter (not . (`S.member` visited)) . maybe [] nub . M.lookup x $ m
  put (visited `S.union` S.fromList xs)
  pure xs

exploreAll :: Ord a => M.Map a [a] -> [a] -> State (S.Set a) [[a]]
exploreAll m xs =
  fmap (xs:) (exploreAll m =<< (fmap concat (traverse (explore m) xs)))

orbit :: Ord a => M.Map a [a] -> a -> [a]
orbit m x =
  concat . takeWhile (not . null)
  . evalState (exploreAll m [x]) . S.singleton $ x

nextOrbit :: (Show a, Ord a) => M.Map a [a] -> S.Set a -> Maybe ([a],S.Set a)
nextOrbit m visited =
  fmap f (listToMaybe $ S.toList (M.keysSet m `S.difference` visited))
  where f x = let os = orbit m x in (os, S.fromList os `S.union` visited)

buildMap :: Ord a => (a,[a]) -> M.Map a [a]
buildMap (x,ys) =
  foldl (M.unionWith (++)) (M.singleton x ys) . fmap (`M.singleton` [x]) $ ys

parsePipe :: Integral a => Parser (M.Map a [a])
parsePipe = do
  x <- fmap round scientific
  skipSpace
  string "<->"
  skipSpace
  ys <- sepBy (fmap round scientific) (string ", ")
  pure (buildMap (x,ys))

buildWorld :: Integral a => FilePath -> IO (M.Map a [a])
buildWorld filePath = do
  Right pipes <- fmap (sequence . fmap (parseOnly parsePipe) . T.lines . T.pack)
                 . readFile $ filePath
  pure (foldl (M.unionWith (++)) M.empty pipes)

part1 :: FilePath -> IO Int
part1 filePath = do
  world <- buildWorld filePath
  pure . length . orbit world $ 0

part2 :: FilePath -> IO Int
part2 filePath = do
  world <- buildWorld filePath
  pure (length . unfoldr (nextOrbit world) $ S.empty)
