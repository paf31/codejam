module Main where

import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

type Case = (Int, Int, [[Int]])

indexed :: [[a]] -> M.Map (Int, Int) a
indexed = M.fromList . concat . zipWith (\i xs -> map (\(j, x) -> ((i, j), x)) xs) [0..] . map (zip [0..])

existsPath :: Int -> Int -> Int -> Int -> M.Map (Int, Int) Int -> Bool
existsPath w h i j m = 
  let
    height = fromJust $ M.lookup (i, j) m
  in
    all (<= height) [ fromJust $ M.lookup (i, j') m | j' <- [0..w-1] ] ||
    all (<= height) [ fromJust $ M.lookup (i', j) m | i' <- [0..h-1] ]  

solve :: Case -> Bool
solve (w, h, c) = let map = indexed c in all (\((i, j), _) -> existsPath w h i j map) (M.toList map)

splitCases :: [String] -> [Case]
splitCases [] = []
splitCases (intro:rest) = let (h, w) = parseIntro intro in ((w, h, parseCase (take h rest)) : splitCases (drop h rest))
  where
  parseIntro :: String -> (Int, Int)
  parseIntro = (\[h, w] -> (h, w)) . map read . splitOn " "
  parseCase :: [String] -> [[Int]]
  parseCase = map $ map read . splitOn " "
  
yesNo :: Bool -> String
yesNo True = "YES"
yesNo False = "NO"

main = interact (unlines . map (\(n, b) -> "Case #" ++ show n ++ ": " ++ yesNo b) . zip [1..] . map solve . splitCases . drop 1 . lines)