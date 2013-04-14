import Data.List ( nub )

solve :: String -> String
solve = show . solve' . map read . words where
  solve' :: [Int] -> Int
  solve' (_:s:p:ts) =
    let numEligibleCases = eligibleCases ts 1 p in
    let numEligibleSurprisingCases = eligibleCases ts 2 p in
    let requiresSurprisingCase = numEligibleSurprisingCases - numEligibleCases in
    let surprisingCases = min requiresSurprisingCase s in
    surprisingCases + numEligibleCases

eligibleCases ts d p = length $ filter (not . null . (cases d p)) ts

cases d p t = [ 1 | 
  a <- [0..10], 
  b <- [0..10], 
  c <- [0..10], 
  a + b + c == t, 
  abs (a - b) <= d, 
  abs (b - c) <= d, 
  abs (c - a) <= d, 
  a >= p || b >= p || c >= p ]

main = interact (unlines . map (\(n, s) -> "Case #" ++ show n ++ ": " ++ s) . zip [1..] . map solve . drop 1 . lines)