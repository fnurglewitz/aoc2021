{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.Array ( Ix(inRange), Array, listArray, bounds, (!), elems )
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text
import Data.Bool (bool)
import Data.Tree (drawForest)

type Pos = (Int, Int)

type Board = Array Pos (Int, Bool)

{-# inline  uat #-}
uat :: Board -> Pos -> Bool
uat a p
  | inRange (bounds a) p = snd $ a ! p
  | otherwise = error "FAIL"

parseInt :: Parser Int
parseInt = do
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return (fst n)
    Left _ -> error "parseInt: should not be here"

parseDraws :: Parser [Int]
parseDraws = many1 $ do
    n <- parseInt
    optional $ char ','
    return n

parseBoard :: Parser Board
parseBoard = do
    rows@(r:rs) <- count 5 $ parseRow >>= \rw -> optional newline >> return rw
    skipMany newline
    return $ listArray ((1, 1), (length rows,length r)) $ (,False) <$> concat rows
    where
        parseRow = count 5 $ do
                    count 2 . optional $ char ' '
                    n <- parseInt
                    count 2 . optional $ char ' '
                    return n

gameParser :: Parser ([Int], [Board])
gameParser = do
    draws <- parseDraws
    many1 newline
    boards <- many1 parseBoard
    return (draws, boards) 

day4 :: IO ()
day4 = do
    input <- readFile "data/day4/day4.txt"
    case parse gameParser "day4" (T.pack input) of
        Left err -> error "shit"
        Right res@(draws, boards) -> do
            let (ln, wb) = runGame draws boards
                unmarked = sum $ fst <$> filter (not . snd) (elems wb)

                part2 = runGame2 draws boards
                (p2draw, p2winners) = last $ filter (not . null . snd) part2
                p2unmarked = sum $ fst <$> filter (not . snd) (elems . last $ p2winners)
            print $ ln*unmarked
            print $ p2draw * p2unmarked

runGame :: [Int] -> [Board] -> (Int, Board)
runGame [] _ = error "Couldn't find winner"
runGame (d:ds) boards = do
    let applied = applyN d <$> boards
        winner = filter isWinner applied
        in if not (null winner) then (d, head winner) else runGame ds applied

runGame2 :: [Int] -> [Board] -> [(Int, [Board])]
runGame2 [] _ = []
runGame2 (d:ds) bs = do
            let applied = applyN d <$> bs
                loser = filter (not . isWinner) applied
                winner = filter isWinner applied
                in (d, winner) : runGame2 ds loser

applyN :: Int -> Board -> Board
applyN n b = (\(m,o) -> (m, o || n == m)) <$> b

isWinner :: Board -> Bool
isWinner board = let
    rows = [ [ (x,y) | y <- [1..5]] | x <- [1..5] ]
    cols = [ [ (y,x) | y <- [1..5]] | x <- [1..5] ]
    in or $ check board <$> (rows ++ cols)
    where
        check :: Board -> [Pos] -> Bool
        check b p = and $ uat board <$> p