solve :: [String] -> [Int]
solve [] = []
solve (header:body) = 
  let (w, h, d) = parseHeader header in
  let (lines, rest) = splitAt h body in
  (solveCase w h d lines : solve rest)

parseHeader = parseHeader' . map read . words where
  parseHeader' (w:h:d:[]) = (w, h, d)

solveCase :: Int -> Int -> Int -> [String] -> Int
solveCase w h d text = 
  let map = buildMap text in
  let neighbours = findNeighbours w h map in
  let eventList = events w h d neighbours in
  solveMap w h d eventList

data CellType = Empty | Mirror | Self

mirror Mirror = True
mirror _ = False

buildMap :: [String] -> [[CellType]]
buildMap = map (map buildCell) where
  buildCell '#' = Mirror
  buildCell 'X' = Self
  buildCell '.' = Empty

data NeighbourType = T | L | B | R | TL | TR | BL | BR

type Neighbours = NeighbourType -> Bool

findNeighbours :: Int -> Int -> [[CellType]] -> [[(CellType, Neighbours)]]
findNeighbours w h cells = [[ (cells !! j !! i, neighbours i j cells) | i <- [0..w-1]] | j <- [0..h-1]] where
  neighbours i j cells T = j > 0 && (mirror $ cells !! (j - 1) !! i)
  neighbours i j cells L = i > 0 && (mirror $ cells !! j !! (i - 1))
  neighbours i j cells B = j < h - 1 && (mirror $ cells !! (j + 1) !! i)
  neighbours i j cells R = i < w - 1 && (mirror $ cells !! j !! (i + 1))
  neighbours i j cells TL = i > 0 && j > 0 && (mirror $ cells !! (j - 1) !! (i - 1))
  neighbours i j cells TR = i < w - 1 && j > 0 && (mirror $ cells !! (j - 1) !! (i + 1))
  neighbours i j cells BL = i > 0 && j < h - 1 && (mirror $ cells !! (j + 1) !! (i - 1))
  neighbours i j cells BR = i < w - 1 && j < h - 1 && (mirror $ cells !! (j + 1) !! (i + 1))

data EventType = WallH | WallV | CornerTL | CornerTR | CornerBL | CornerBR | StartingPoint

data Event = Event EventType Rational Rational

events :: Int -> Int -> Int -> [[(CellType, Neighbours)]] -> [Event]
events w h d cells = selfEvents w h cells ++ wallEvents w h cells ++ cornerEvents w h cells

selfEvents w h cells = [Event Self (i / 2) (j / 2)] where 

solveMap :: Int -> Int -> Int -> [Event] -> Int
solveMap w h d cells = undefined

printCase n s = "Case #" ++ show n ++ ": " ++ show s

main = interact (unlines . map (uncurry printCase) . zip [1..] . solve . drop 1 . lines)