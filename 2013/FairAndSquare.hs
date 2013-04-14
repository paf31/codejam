module Main where

import Data.List
import Data.List.Split
    
firstFew :: [Integer]
firstFew = [1,2,11,22,101,202,111,212,121,1111,11011,11111,11211,111111,1110111,1111111,11111111,111101111,111111111]
     
ones :: Int -> Integer
ones 1 = 1
ones 2 = 11
ones 3 = 111
ones 4 = 1111
ones 5 = 11111
ones n = ones (n - 5) * 100000 + 11111
     
fairAndSquareNumbers :: [Integer]
fairAndSquareNumbers = firstFew ++ filter (\n -> isPalindrome n && isPalindrome (n * n)) [ let o = ones nzd in o + o * (10 ^ (d * 2 + nzd)) | d <- [0..50], nzd <- [1..9] ]
    
isPalindrome :: Integer -> Bool
isPalindrome n = let s = show n in reverse s == s

fairAndSquareNumbersBetween :: Integer -> Integer -> [Integer]
fairAndSquareNumbersBetween l h = 
  [ n | n <- fairAndSquareNumbers, l <= n * n, n * n <= h ]
    
solve :: String -> Int
solve = (\[l, h] -> length $ fairAndSquareNumbersBetween l h) . take 2 . map read . splitOn " "

main = interact (unlines . map (\(n, m) -> "Case #" ++ show n ++ ": " ++ show m) . zip [1..] . map solve . drop 1 . lines)