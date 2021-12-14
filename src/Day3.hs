module Day3 where

import Data.Char (digitToInt)
import Data.List (transpose,group,sort,sortBy,nub)
import Debug.Trace
import GHC.Generics (Associativity(LeftAssociative))
import Text.ParserCombinators.Parsec (digit)
import Utils.Containers.Internal.BitUtil (bitcount)

day3 :: IO ()
day3 = do
    input <- lines <$> readFile "data/day3/day3.txt"
    let columns = transpose input
        groupedCols = (fmap . fmap) concat $ group . sortBy (\x y -> length x `compare` length y) .  group . sort <$> columns
        commons = toTuple <$> groupedCols
        lcv = fst <$> commons
        mcv = snd <$> commons
        oxygen = toInt $ rating input 0 mcv (>=)
        co2 = toInt $ rating input 0 lcv (<)
    print $ toInt mcv * toInt lcv
    print $ oxygen*co2

toTuple :: [String] -> (Char, Char)
toTuple [x@(x':_)] = (x',x')
toTuple (x@(x':_):y@(y':_):_) = (x',y')

toInt :: String -> Int
toInt "" = 0
toInt xs  = foldl (\b a -> digitToInt a + b*2) 0 xs

-- this shit sucks as hell
rating :: [String] -> Int -> String -> (Int -> Int -> Bool) -> String
rating ns ix (m:ms) cmp = let
        [zeroes,ones] = fmap length . group . sort $ (!! ix) <$> ns
        target = if ones `cmp` zeroes then '1' else '0'
        rs = filter (\str -> str !! ix == target) ns
    in if length rs == 1 then head rs else rating rs (ix+1) ms cmp