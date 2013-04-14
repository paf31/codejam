module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.MemoTrie

type Chest = (Int, [Int], Bool)

requiredKey (x,_,_) = x
keysInside (_,x,_) = x
opened (_,_,x) = x

open (x,y,_) = (x,y,True)

type Case = ([Int], [Chest])

keys = fst
chests = snd

splitCases :: [String] -> [Case]
splitCases [] = []
splitCases (intro:ks:rest) = let (k, n) = parseIntro intro in ((parseKeys ks, map parseChest (take n rest)) : splitCases (drop n rest))
  where
  parseIntro :: String -> (Int, Int)
  parseIntro = (\[k, n] -> (k, n)) . map read . splitOn " "
  parseKeys :: String -> [Int]
  parseKeys = map read . splitOn " "
  parseChest :: String -> Chest
  parseChest = (\(ti:_:keys) -> (ti, keys, False)) . map read . splitOn " "

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f = (\(head, (x:tail)) -> head ++ (f x:tail)) . splitAt n

shortest :: [[a]] -> [[a]]
shortest xss = let len = minimum $ map length xss in
               filter (\xs -> length xs == len) xss
  
allPaths :: Case -> [[Int]]
allPaths c 
  | all opened $ chests c = [[]]
  | otherwise = maybeToList . listToMaybe . sort $
  [ (index:indices) | (chest, index) <- chests c `zip` [0..], 
              not $ opened chest, 
              requiredKey chest `elem` keys c, 
              indices <- allPaths 
                              ((keys c \\ [requiredKey chest]) ++ keysInside chest, 
                               modifyAt index open (chests c))  ]

solve :: Case -> Maybe [Int]
solve c = listToMaybe . sort . memo allPaths $ c
  
showSolution :: Maybe [Int] -> String
showSolution = maybe "IMPOSSIBLE" (intercalate " " . map show)

main = interact (unlines . map (\(n, m) -> "Case #" ++ show n ++ ": " ++ showSolution m) . zip [1..] . map solve . splitCases . drop 1 . lines)