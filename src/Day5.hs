{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Day5 where

import Data.List (group,sort)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text

type Pos = (Int, Int)

day5 :: IO ()
day5 = do
    input <- readFile "data/day5/day5.txt"
    case parse (many1 parseRange) "day5" (T.pack input) of
        Left err -> error "shit"
        Right x -> do
            let positions = concat $ toRange <$> x
                positions' = concat $ toRange' <$> x
            print $ length $ filter (>1) $ fmap length $ group . sort $ positions
            print $ length $ filter (>1) $ fmap length $ group . sort $ positions'
            return ()

parseInt :: Parser Int
parseInt = do
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return (fst n)
    Left _ -> error "parseInt: should not be here"

parseRange :: Parser (Pos,Pos)
parseRange = do
    x <- parseInt
    char ','
    y <- parseInt
    string " -> "
    x' <- parseInt
    char ','
    y' <- parseInt
    optional newline
    return ((x,y),(x',y'))


findSize :: [(Pos,Pos)] -> Pos
findSize = go (0,0)
    where
        max' = foldr max 0
        go acc [] = acc
        go (ax,ay) (((x,y),(x',y')):xys) = go (max' [ax,x,x'], max' [ay,y,y']) xys

toRange :: (Pos,Pos) -> [Pos]
toRange ((x,y),(x',y'))
    | x == x' && y <= y' = (x,) <$> [y..y']
    | x == x' && y > y' = (x,) <$> [y'..y]
    | y == y' && x <= x' = (,y) <$> [x..x']
    | y == y' && x > x' = (,y) <$> [x'..x]
    | otherwise = []

toRange' :: (Pos,Pos) -> [Pos]
toRange' ((x,y),(x',y'))
    | x == x' && y <= y' = (x,) <$> [y..y']
    | x == x' && y > y' = (x,) <$> [y'..y]
    | y == y' && x <= x' = (,y) <$> [x..x']
    | y == y' && x > x' = (,y) <$> [x'..x]
    | otherwise = [ (a,b) | a <- r x x' | b <- r y y' ]

r :: Int -> Int -> [Int]
r a b
    | a <= b = [a..b]
    | otherwise = [a,a-1..b]
