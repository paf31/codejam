module Main where

import Data.MemoTrie
import Data.Function
import Data.List
import Data.List.Split

data Case = Case Integer Integer [Integer]

splitCases :: [String] -> [Case]
splitCases [] = []
splitCases (l1:l2:rest) = 
  let 
    (e, r, _) = (\(e:r:n:_) -> (e, r, n)) $ map read $ splitOn " " l1 
    vs = map read $ splitOn " " l2
  in 
    (Case e r vs : splitCases rest)

deleteAt i xs = let (ys, z:zs) = splitAt i xs in ys ++ zs
    
solve :: Case -> Integer
solve (Case te r vs) = memo2 solve' te vs
  where
  solve' e _ | e <= 0 = 0
  solve' _ [] = 0
  solve' e (v:vs) = 
    maximum [ v * e' + solve' (min te (e - e' + r)) vs | e' <- [0..e] ]

main = interact (unlines . map (\(n, b) -> "Case #" ++ show n ++ ": " ++ show b) . zip [1..] . map solve . splitCases . drop 1 . lines)
 