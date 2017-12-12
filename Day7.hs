{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List (findIndex,partition)
import qualified Data.Map as M
import Data.Maybe
import Data.Attoparsec.Text
import Data.Text hiding (findIndex,foldl,partition)

import Prelude hiding (lines)

data Tree a = Tree a Integer [Tree a] deriving Show

buildParentMap :: Ord a => [(a,[a])] -> M.Map a a
buildParentMap = foldMap build
  where build (parent,children) = foldMap (flip M.singleton parent) children

findRoot :: Ord a => M.Map a a -> Maybe a
findRoot parentMap = case M.minViewWithKey parentMap of
  Just ((child,parent),_) -> Just (climb parent)
  Nothing -> Nothing
  where climb x = case M.lookup x parentMap of
                    Nothing -> x
                    Just parent -> climb parent

invertMap :: (Ord a, Ord b) => M.Map a b -> M.Map b [a]
invertMap = foldl (M.unionWith (++)) M.empty
            . fmap (\(x,y) -> M.singleton y [x])
            . M.assocs

buildTree :: Ord a => (a,Integer) -> M.Map (a,Integer) [(a,Integer)] -> Tree a
buildTree (root,weight) childMap =
  case M.lookup (root,weight) childMap of
  Nothing -> Tree root weight []
  Just cs -> Tree root weight (fmap (flip buildTree childMap) cs)

oddOneOut :: Eq a => [a] -> Maybe (a, Int, a)
oddOneOut [] = Nothing
oddOneOut (x:xs) =
  case partition (== x) (x:xs) of
  ([x'],(y':z':_)) -> fmap (\i -> (x',i,y')) (findIndex (== x') (x:xs))
  ((y':z':_),[x']) -> fmap (\i -> (x',i,y')) (findIndex (== x') (x:xs))
  _ -> Nothing

balance :: Tree a -> Either Integer Integer
balance (Tree _ weight []) = Right weight
balance (Tree _ weight children) =
  case traverse balance children of
  Left w -> Left w
  Right ws -> case oddOneOut ws of
    Just (oddOne,oddIndex,rest) ->
      let Tree _ oddWeight _ = children !! oddIndex in
      Left (oddWeight + rest - oddOne)
    Nothing -> Right (weight + sum ws)

parseName :: Parser String
parseName = many1 letter

parseData :: Parser (String,Integer,[String])
parseData = do
  name <- parseName
  skipSpace
  char '('
  weight <- scientific
  char ')'
  skipSpace
  names <- option [] $ do
    string "->"
    skipSpace
    sepBy parseName (char ',' >> skipSpace)
  pure (name,round weight,names)

readData :: FilePath -> IO [(String, Integer, [String])]
readData file = do
  Right tree <- fmap (sequence . fmap (parseOnly parseData) . lines . pack)
                $ readFile file
  pure tree

part1 :: FilePath -> IO (Maybe String)
part1 file = do
  tree <- readData file
  let parents =
        buildParentMap [ (parent,children) | (parent,_,children) <- tree ]
  pure (findRoot parents)

part2 :: FilePath -> IO (Either Integer Integer)
part2 file = do
  tree <- readData file
  let parents =
        buildParentMap [ ((parent,children))
                       | (parent,_,children) <- tree ]
  let weightMap = M.fromList [ (parent,weight)
                             | (parent,weight,_) <- tree ]
  let addWeight n = (n, fromJust (M.lookup n weightMap))
  let weightParents = M.mapKeys addWeight . M.map addWeight $ parents
  let Just root = findRoot weightParents
  pure . balance . buildTree root . invertMap $ weightParents
