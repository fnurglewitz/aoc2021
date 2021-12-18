module Day7 where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text

parseInt :: Parser Int
parseInt = do
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return (fst n)
    Left _ -> error "parseInt: should not be here"

parser :: Parser [Int]
parser = many1 $ do
    x <- parseInt
    optional $ char ','
    return x
    
day7 :: IO ()
day7 = do
    input <- readFile "data/day7/day7.txt"
    case parse parser "day7" (T.pack input) of
        Left err -> error "shit"
        Right x -> do
            let sorted = L.sort x
                min = head sorted
                max = last sorted
            print $ minimum $ sum <$> [ [ abs $ x-y | x <- x ] | y <- [min..max] ]
            print $ minimum $ sum <$> [ [ sum [1..abs $ x-y] | x <- x ] | y <- [min..max] ]
