{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Day11 where

import Data.Array ( Ix(inRange), Array, listArray, bounds, (!), indices, accum, elems, assocs)
import Data.Char(digitToInt)
import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int,Int)
type Grid = Array Pos Int

day11 :: IO ()
day11 = do
    input@(x:xs) <- (fmap . fmap $ digitToInt) . lines <$> readFile "data/day11/day11.txt"
    let grid = listArray ((1, 1), (length input,length x)) $ concat input
    print $ part1 grid
    print $ part2 grid

part1 :: Grid -> Int
part1 = go 100 
    where
        go 0 _ = 0
        go x g = let (fc, g') = step g in fc + go (x-1) g'

part2 :: Grid -> Int
part2 = go 1
    where
        go x g = let (fc, g') = step g in if fc == 100 then x else go (x+1) g'

neighbors :: M.Map Pos [Pos]
neighbors = let ps = [ (x,y) | x <- [1..10], y <- [1..10]]
    in M.fromList $ zip ps (adj <$> ps)
    where
        adj (y,x) = let
            n = (y-1, x)
            s = (y+1, x)
            e = (y, x+1)
            w = (y, x-1)
            ne = (y-1, x+1)
            nw = (y-1, x-1)
            se = (y+1, x+1)
            sw = (y+1, x-1)
            in filter (\(x,y) -> x >= 1 && x <= 10 && y >= 1 && y <= 10) [n,s,e,w,ne,nw,se,sw]

step :: Grid -> (Int, Grid)
step grid = let grid' = (+1) <$> grid in reset <$> explode grid' S.empty (flashing grid')
    where
        flashing :: Grid -> S.Set Pos
        flashing = S.fromList . fmap fst . filter ((>9) . snd) . assocs
        explode :: Grid -> S.Set Pos -> S.Set Pos -> (Int, Grid)
        explode g v f
            | S.null f = (S.size v, g)
            | otherwise = let
                v' = S.union v f
                n = concatMap (neighbors M.!) (S.toList f)
                g' = accum (+) g $ zip n (repeat 1)
                in explode g' v' $ flashing g' S.\\ v'
        reset :: Grid -> Grid
        reset = fmap (\x -> if x > 9 then 0 else x)
