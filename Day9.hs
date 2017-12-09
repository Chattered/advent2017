import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.ByteString as BS
import Data.Text.Encoding

scoreGarbage :: Integer -> Parser Integer
scoreGarbage n =
  choice [char '!' >> anyChar >> scoreGarbage n
         ,char '<' >> skipGarbage
         ,char '{'
          *> fmap ((+ n) . sum) (sepBy (scoreGarbage (n+1)) (char ','))
          <* char '}']
  where skipGarbage = manyTill ((char '!' >> anyChar) <|> anyChar) (char '>')
                      *> pure 0
        getGarbage = scoreGarbage (n+1) <* char '}'

countGarbage :: Parser Integer
countGarbage =
    choice [char '!' >> anyChar >> countGarbage
           ,char '<' >> countToEnd
           ,char '{' *> fmap sum (sepBy countGarbage (char ',')) <* char '}']
  where countToEnd =
          choice [char '>' >> pure 0
                 ,liftA2 (+) garbageChar countToEnd]
        garbageChar = choice [char '!' >> anyChar >> pure 0
                             ,anyChar >> pure 1]

part1 :: FilePath -> IO (Either String Integer)
part1 file = parseOnly (scoreGarbage 1) . decodeUtf8 <$> BS.readFile file

part2 :: FilePath -> IO (Either String Integer)
part2 file = parseOnly countGarbage . decodeUtf8 <$> BS.readFile file
