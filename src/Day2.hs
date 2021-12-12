{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Error

data Command = Forward Int | Down Int | Up Int deriving (Show)

toOffset :: Command -> (Int, Int)
toOffset (Forward x) = (0, x)
toOffset (Down x) = (x, 0)
toOffset (Up x) = (-x, 0)

doCommand :: Command -> (Int, Int, Int) -> (Int, Int, Int)
doCommand (Down d) (x,y,aim) = (x,y,aim+d)
doCommand (Up u) (x,y,aim) = (x,y,aim-u)
doCommand (Forward f) (x,y,aim) = (x+f,y+(aim*f), aim)

zum :: (Int, Int) -> (Int, Int) -> (Int, Int)
zum (x,y) (a,b) = (x+a,y+b)

parseInt :: Parser Int
parseInt = do
  num <- T.pack <$> many1 digit
  case TR.decimal num of
    Right n -> return (fst n)
    Left _ -> error "parseInt: should not be here"


cmdParser :: String -> (Int -> Command) -> Parser Command
cmdParser lbl ctor = do
    string lbl
    ctor <$> parseInt

commandParser :: Parser Command
commandParser = try (cmdParser "forward " Forward)
    <|> try (cmdParser "forward " Forward)
    <|> try (cmdParser "down " Down)
    <|> try (cmdParser "up " Up)

day2 :: IO ()
day2 = do
    input <- readFile "data/day2/day2.txt"
    case parse parser "day2" (T.pack input) of
        Right x -> do
            print $ (*) <$> fst <*> snd $ foldr zum (0,0) $ toOffset <$> x
            print $ let (pos,depth,aim) = foldr doCommand (0,0,0) (reverse x) in pos*depth
        Left err -> print err
    where
        parser = many (optional newline >> commandParser)