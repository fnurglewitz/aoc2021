module Day6 where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text

type Population = M.Map Int Int

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
    
day6 :: IO ()
day6 = do
    input <- readFile "data/day6/day6.txt"
    case parse parser "day6" (T.pack input) of
        Left err -> error "shit"
        Right x -> do
            print $ sum $ M.elems $ run 80 (start x)
            print $ sum $ M.elems $ run 256 (start x)

start :: [Int] -> Population
start xs = populate xs M.empty  
    where
        populate [] pop = pop
        populate (x:xs) pop = populate xs $ M.unionWith (+) pop (M.singleton x 1)

rules :: Int -> [Int]
rules 0 = [6, 8]
rules n = [n - 1]

step :: Population -> Population
step p = foldr (M.unionWith (+)) M.empty $ do
  (g, n) <- M.assocs p
  g' <- rules g
  pure $ M.singleton g' n

run :: Int -> Population -> Population
run 0 pop = pop
run n pop = run (n-1) (step pop)
