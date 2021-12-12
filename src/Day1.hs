module Day1 where

import qualified Data.Monoid  as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

day1 :: IO ()
day1 = do
    depths <- (fmap . fmap) (r . T.unpack) (T.lines <$> TIO.readFile "data/day1/day1.txt")
    print $ f 1 depths
    print $ f 3 depths
    where
        r :: String -> Int
        r = read
        f :: Ord a => Int -> [a] -> Int
        f n x = length . filter id $ zipWith (<) <*> drop n $ x