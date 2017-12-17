{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import Data.Array.IO
import Data.IORef
import Data.Foldable
import qualified Data.Text as T

data Cursor = Cursor !(IOUArray Int Char) !(IOUArray Char Int) (IORef Int)

resolveIndex :: Cursor -> Int -> IO Int
resolveIndex (Cursor chars positions r) i = do
  j <- readIORef r
  b@(l,_) <- getBounds chars
  pure $ ((i + j) `rem` (rangeSize b)) + l

showCursor :: Cursor -> IO String
showCursor (Cursor chars _ r) = do
  xs <- getElems chars
  i <- readIORef r
  s <- fmap rangeSize (getBounds chars)
  pure . take s . drop i . cycle $ xs

lookupResolve :: Cursor -> Int -> IO Char
lookupResolve c@(Cursor arr _ _) i = resolveIndex c i >>= readArray arr

put :: Cursor -> Int -> Char -> IO ()
put c@(Cursor chars poss _) i e = do
  i' <- resolveIndex c i
  writeArray chars i' e
  writeArray poss e i'

spin :: Int -> Cursor -> IO ()
spin i c@(Cursor chars _ r) = do
  s <- fmap rangeSize (getBounds chars)
  i' <- resolveIndex c (s - i)
  writeIORef r i'

exchange :: Int -> Int -> Cursor -> IO ()
exchange m n cursor = do
  x <- lookupResolve cursor m
  y <- lookupResolve cursor n
  put cursor m y
  put cursor n x

partner :: Char -> Char -> Cursor -> IO ()
partner x y c@(Cursor chars poss _) = do
  m <- readArray poss x
  n <- readArray poss y
  writeArray chars m y
  writeArray chars n x
  writeArray poss x n
  writeArray poss y m

parseDance =
  (char 's' >> fmap spin integer)
  <|> (char 'x' >> liftA2 exchange (integer <* char '/') integer)
  <|> (char 'p' >> liftA2 partner anyChar (char '/' *> anyChar))
  where integer = fmap round scientific

readDance :: FilePath -> IO (Cursor -> IO ())
readDance = fmap (either (const . putStr) f
                  . parseOnly (sepBy parseDance (char ','))
                  . T.pack)
            . readFile
  where f xs c = traverse_ ($ c) xs

untilM :: (Monad m, Eq a) => (a -> Bool) -> m a -> m [a]
untilM p act = do
  x <- act
  if p x then pure [] else fmap (x :) (untilM p act)

initCursor :: IO Cursor
initCursor = do
  chars <- newListArray (0,15) ['a'..'p']
  poss <- newListArray ('a','p') [0..15]
  r <- newIORef 0
  pure (Cursor chars poss r)

part1 :: FilePath -> IO String
part1 filePath = do
  c <- initCursor
  dance <- readDance filePath
  dance c
  showCursor c

part2 :: FilePath -> IO String
part2 filePath = do
  c <- initCursor
  dance <- readDance filePath
  xs <- showCursor c
  repeatNo <- fmap length (untilM (== xs) (dance c >> showCursor c))
  replicateM (1000000000 `rem` (repeatNo + 1)) (dance c)
  showCursor c
