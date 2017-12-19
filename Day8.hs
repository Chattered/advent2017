import Control.Applicative
import Data.Attoparsec.Text
import Data.List (mapAccumL)
import qualified Data.Map as M
import Data.Text (lines,pack)

import Prelude hiding (lines)

data Instr r = Instr r (Integer -> Integer) r (Integer -> Bool)

parseInstr :: Parser (Instr String)
parseInstr = do
  reg <- many1 letter

  skipSpace
  op <- ((string "inc" *> pureFlip (+)) <|> (string "dec" *> pureFlip (-)))
        <*> (skipSpace >> fmap round scientific)

  regCond <- skipSpace >> string "if" >> skipSpace >> many1 letter

  skipSpace
  cmp <- choice
         [ string "==" *> pureFlip (==)
         , string "!=" *> pureFlip (/=)
         , string ">=" *> pureFlip (>=)
         , string "<=" *> pureFlip (<=)
         , string ">" *> pureFlip (>)
         , string "<" *> pureFlip (<)
         ] <*> (skipSpace >> fmap round scientific)

  pure (Instr reg op regCond cmp)
  where pureFlip = pure . flip

evalInstr :: Ord a => Instr a -> M.Map a Integer -> M.Map a Integer
evalInstr (Instr r op s cmp) m =
  let (x,m') =
        case M.lookup s m of
        Nothing -> (0,M.insert s 0 m)
        Just x' -> (x',m) in
  if cmp x then M.alter (f op) r m' else m'
  where f op Nothing  = Just (op 0)
        f op (Just x) = Just (op x)

cumulate :: Traversable t => (b -> a -> b) -> b -> t a -> (b, t b)
cumulate f = mapAccumL (\acc x -> (f acc x, acc))

part1 :: FilePath -> IO Integer
part1 file = do
  Right instrs <- fmap (traverse (parseOnly parseInstr) . lines . pack)
                  $ readFile file
  pure . maximum . M.elems . foldl (flip evalInstr) M.empty $ instrs

part2 :: FilePath -> IO Integer
part2 file = do
 Right instrs <- fmap (traverse (parseOnly parseInstr) . lines . pack)
                 $ readFile file
 let (lm,ms) = cumulate (flip evalInstr) M.empty $ instrs
 pure . maximum . concatMap M.elems $ (lm:ms)
