{-# LANGUAGE ViewPatterns #-}

module Day8 where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Monoid as MO
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Text
import qualified GHC.OverloadedLabels as S
import qualified Control.Lens.At as L
import qualified GHC.OverloadedLabels as M

parseLine :: Parser ([String], [String])
parseLine = do
    ptns <- count 10 $ do
        optional $ char ' '
        many1 $ oneOf "abcdefg"
    string " |"
    out <- count 4 $ do
        optional $ char ' '
        many1 $ oneOf "abcdefg"
    optional newline
    return (ptns,out)

parser :: Parser [([String], [String])]
parser = many1 parseLine

day8 :: IO ()
day8 = do
    input <- readFile "data/day8/day8.txt"
    case parse parser "day8" (T.pack input) of
        Left err -> error (show err)
        Right x -> do
            print $ length $ filter isJust $ concat $ fmap simpleDecode . snd <$> x
            print $ sum $ decode <$> x

simpleDecode :: String -> Maybe (MO.Sum Int)
simpleDecode (length -> l)
    | l == 2 = pure . pure $ l
    | l == 4 = pure . pure $ l
    | l == 3 = pure . pure $ l
    | l == 7 = pure . pure $ l
    | otherwise = Nothing

decode :: ([String], [String]) -> Int
decode (digits, vals) = let
    sets = S.fromList <$> digits
    valSets = S.fromList <$> vals
    one = head $ filter ((==2) . length) sets
    four = head $ filter ((==4) . length) sets
    seven = head $ filter ((==3) . length) sets
    eight = head $ filter ((==7) . length) sets
    remaining = filter (\s -> s /= one && s /= four && s /= seven && s /= eight ) sets -- 0 2 3 5 6 9
    six = head $ filter (not . S.null) $ filter (\s -> length s == 6 && not (S.null (one S.\\ s))) remaining
    remaining' = filter (/= six) remaining -- 0 2 3 5 9
    zero = head $ filter (not . S.null) $ filter (\s -> length s == 6 && not (S.null (four S.\\ s))) remaining'
    remaining'' = filter (/= zero) remaining' -- 2 3 5 9
    nine = head $ filter ((==6) . length) remaining''
    five = head $ filter (not . S.null) $ filter (\s -> length s == 5 && length (six S.\\ s) == 1) remaining''
    remaining''' = filter (\s -> s /= five && s /= nine ) remaining'' -- 2 3
    three = head $ filter (\s -> length (nine S.\\ s) == 1) remaining'''
    two = head $ filter (/= three) remaining'''
    symbols = M.fromList [(zero, 0),(one, 1),(two, 2),(three, 3),(four, 4),(five, 5),(six, 6),(seven, 7),(eight, 8),(nine, 9)]
    in foldl (\a b -> a*10 + b) 0 $ (M.!) symbols <$> valSets
