{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Day10 where

import Data.Either ( rights )
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Monoid as MO
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text

data Chunk = Round [Chunk] | Square [Chunk] | Curly [Chunk] | Angle [Chunk] | Incomplete Char [Chunk]
    deriving (Show)

parseChunks :: Parser Chunk
parseChunks = do
    try (parseChunk ('(',')') Round)
    <|> try (parseChunk ('[',']') Square)
    <|> try (parseChunk ('{','}') Curly)
    <|> try (parseChunk ('<','>') Angle)

parseChunk :: (Char,Char) -> ([Chunk] -> Chunk) -> Parser Chunk
parseChunk (o,c) ctor = do
    char o
    cs <- many parseChunks
    try (closed c cs) <|> incomplete o cs
    where
        closed c cs = char c >> return (ctor cs)
        incomplete o cs = eof >> return (Incomplete (opp o) cs)

opp :: Char -> Char
opp '(' = ')'
opp '[' = ']'
opp '{' = '}'
opp '<' = '>'

day10 :: IO ()
day10 = do
    input <- lines <$> readFile "data/day10/day10.txt"
    let parsed = parze <$> input
        errors = map toErrorPos parsed
        failures = zipWith toFailure input errors
    print $ sum $ toScore <$> failures
    print $ head $ middle $ L.sort $ toScore2 . concatMap (reverse . foldIncomplete) <$> rights parsed
    where
        parze :: String -> Either ParseError [Chunk]
        parze = parse (many1 parseChunks) "day10" . T.pack

toErrorPos :: Either ParseError b -> Maybe Int
toErrorPos (Right _) = Nothing
toErrorPos (Left pe) = Just $ sourceColumn . errorPos $ pe

toFailure :: String -> Maybe Int -> Maybe Char
toFailure _ Nothing = Nothing
toFailure i (Just e) = Just $ i !! (e-1)

toScore :: Maybe Char -> Int
toScore Nothing = 0
toScore (Just ')') = 3
toScore (Just ']') = 57
toScore (Just '}') = 1197
toScore (Just '>') = 25137

foldIncomplete :: Chunk -> String
foldIncomplete (Round cs) = concat $ foldIncomplete <$> cs
foldIncomplete (Square cs) = concat $ foldIncomplete <$> cs
foldIncomplete (Curly cs) = concat $ foldIncomplete <$> cs
foldIncomplete (Angle cs) = concat $ foldIncomplete <$> cs
foldIncomplete (Incomplete c cs) = c : concat (foldIncomplete <$> cs)

toScore2 :: String -> Int
toScore2 = go 0
    where
        s ')' = 1
        s ']' = 2
        s '}' = 3
        s '>' = 4
        go acc [] = acc
        go acc (x:xs) = go (acc*5 + s x) xs

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l
