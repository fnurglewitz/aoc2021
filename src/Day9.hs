{-# LANGUAGE FlexibleContexts #-}

module Day9 where

import Data.Array ( Ix(inRange), Array, listArray, bounds, (!) )
import Control.Lens (ifoldMap)
import Control.Monad(liftM2)
import Data.Char(digitToInt)
import Data.List (sortBy)
import qualified Data.Map  as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Internal.Builder.Int.Digits (digits)

type Pos = (Int, Int)

type Grid = Array Pos Int

type Neighbors = M.Map Pos [Pos]

day9 :: IO ()
day9 = do
    input@(x:xs) <- (fmap . fmap $ digitToInt) . lines <$> readFile "data/day9/day9.txt"
    let grid = listArray ((1, 1), (length input,length x)) $ concat input
        neighborsMap = neighbors grid
        posWithNeighbors = M.keysSet neighborsMap 
        allNeighbors = S.fromList (concat $ M.elems $ neighbors grid)
        lowPoints = S.toList (posWithNeighbors S.\\ allNeighbors)
    print $ sum $ succ . (grid !) <$> lowPoints
    print $ product $ take 3 . sortBy (flip compare) . fmap length $ explore neighborsMap <$> lowPoints

{-# inline  at #-}
at :: Grid -> Pos -> Maybe Int
at a p
  | inRange (bounds a) p = Just $ a ! p
  | otherwise = Nothing

nb :: Grid -> Pos -> Int -> Neighbors
nb _ _ 9 = mempty -- 9s can't have neighbors with a higher number
nb g (x,y) n = let
    u = (x, y-1)    
    d = (x, y+1)
    l = (x-1, y)
    r = (x+1, y)
    in M.singleton (x,y) $ filter 
        (maybe False (liftM2 (&&) (>=n) (<9)) . at g) -- only get neighbors with a higher number (but smaller than 9)
        [u,d,l,r]

neighbors :: Grid -> Neighbors
neighbors = nb >>= ifoldMap

explore :: Neighbors -> Pos -> Set Pos
explore nb = go mempty
    where
        go s p
            | p `S.member` s = mempty
            | otherwise = let 
                visited = S.insert p s
                toVisit = M.findWithDefault mempty p nb
                in visited <> foldMap (go visited) toVisit
