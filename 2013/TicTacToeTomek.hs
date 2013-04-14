module Main where

import Data.List
import Data.List.Split

data Status = XWins | OWins | Incomplete | Draw

instance Show Status where
  show XWins = "X won"
  show OWins = "O won"
  show Incomplete = "Game has not completed"
  show Draw = "Draw"

data Place = X | O | T | Empty deriving (Show, Eq)

parsePlaces :: [String] -> [[Place]]
parsePlaces = map (map parseChar) 
  where  
  parseChar '.' = Empty 
  parseChar 'O' = O
  parseChar 'X' = X  
  parseChar 'T' = T  

checkHoriz :: Int -> (Place -> Bool) -> [[Place]] -> Bool
checkHoriz n p ps = all p [ (ps !! n !! m) | m <- [0..3] ]

checkVert :: Int -> (Place -> Bool) -> [[Place]] -> Bool
checkVert n p ps = all p [ (ps !! m !! n) | m <- [0..3] ]

checkDiag1 :: (Place -> Bool) -> [[Place]] -> Bool
checkDiag1 p ps = all p [ ps !! n !! n | n <- [0..3] ]

checkDiag2 :: (Place -> Bool) -> [[Place]] -> Bool
checkDiag2 p ps = all p [ ps !! n !! (3 - n) | n <- [0..3] ]
  
checkAny :: (Place -> Bool) -> [[Place]] -> Bool
checkAny p ps = any id $
  [ checkHoriz n p ps | n <- [0..3] ] ++
  [ checkVert n p ps | n <- [0..3] ] ++
  [ checkDiag1 p ps, checkDiag2 p ps ]

gameStatus :: [[Place]] -> Status
gameStatus ps | checkAny (\p -> p == X || p == T) ps = XWins
              | checkAny (\p -> p == O || p == T) ps = OWins
              | null $ filter (== Empty) $ concat ps = Draw
              | otherwise = Incomplete
  
solve :: [String] -> String
solve = show . gameStatus . parsePlaces

main = interact (unlines . map (\(n, s) -> "Case #" ++ show n ++ ": " ++ s) . zip [1..] . map solve . splitEvery 5 . drop 1 . lines)