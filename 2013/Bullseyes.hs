module Main where

import Data.List
import Data.List.Split

data Case = Case Integer Integer

readCase :: String -> Case
readCase = (\[r, t] -> Case r t) . map read . splitOn " "

area r n = 2 * n ^ 2 + 2 * r * n - n

solve :: Case -> Integer
solve (Case r t) = 
  let
    a = 2.0
    b = fromIntegral $ 2 * r - 1
    c = fromIntegral $ -t
    zero = floor $ ((-b) + sqrt(b ^ 2 - 4 * a * c)) / (2.0 * a)
  in 
    if area r zero > t then zero - 1 else zero

main = interact (unlines . map (\(n, b) -> "Case #" ++ show n ++ ": " ++ show b) . zip [1..] . map solve . map readCase . drop 1 . lines)
