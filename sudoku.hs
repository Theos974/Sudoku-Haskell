import Data.List ((\\),nub, transpose)

solve :: [String] -> [String]
solve grid = solveGrid grid

type Grid = [String] 

findEmptyCells :: Grid -> [(Int, Int)]
findEmptyCells grid = [(r, c) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, cell == '-']

possibleValues :: Grid -> (Int, Int) -> [Char]
possibleValues grid (r,c) 
  | current /= '-' = [current]
  | otherwise = "123456789" \\ nub (rowValues ++ colValues ++ blockValues)
  where
    current = (grid!!r)!!c
    rowValues = grid!!r
    colValues = transpose grid !! c
    blockValues = getBlock grid (r,c)
    
getBlock :: Grid -> (Int, Int) -> [Char]
getBlock grid (r, c) = 
  [grid !! r' !! c' | r' <- blockRange r, c' <- blockRange c]
  where
    blockRange x = let start = (x `div` 3) * 3 in [start..start+2]

updateGrid :: Grid -> Grid
updateGrid grid = [[updateCell r c | c <- [0..8] ] | r <- [0..8] ]
  where
    updateCell r c
      | grid !! r !! c /= '-' = grid !! r !! c
      | length possible == 1 = head possible
      | otherwise = '-'
      where
        possible = possibleValues grid (r,c)
        
solveGrid :: Grid -> Grid
solveGrid grid
  | grid == updatedGrid = grid  -- If no changes were made, return the current grid
  | otherwise = solveGrid updatedGrid  -- Otherwise, continue solving
  where
    updatedGrid = updateGrid grid