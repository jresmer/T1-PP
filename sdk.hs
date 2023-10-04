import Data.List (transpose)

-- Define a 9x9 Sudoku board as a list of lists of integers.
type Board = [[Int]]

-- Define a function to print the Sudoku board.
printBoard :: Board -> IO ()
printBoard = mapM_ putStrLn . map (unwords . map show)

-- Check if a value can be placed in a specific cell of the board.
isValid :: Board -> Int -> (Int, Int) -> Bool
isValid board num (row, col) =
  all (\x -> board !! row !! x /= num) [0..8] &&  -- Check row
  all (\x -> board !! x !! col /= num) [0..8] &&  -- Check column
  all (\(r, c) -> board !! r !! c /= num) (boxIndices (row, col))  -- Check 3x3 box

-- Find the indices of the 3x3 box that contains the cell at (row, col).
boxIndices :: (Int, Int) -> [(Int, Int)]
boxIndices (row, col) =
  [(r, c) | r <- [boxRow .. boxRow + 2], c <- [boxCol .. boxCol + 2]]
  where boxRow = (row `div` 3) * 3
        boxCol = (col `div` 3) * 3

-- Solve the Sudoku puzzle using backtracking.
solveSudoku :: Board -> Maybe Board
solveSudoku board = solve (emptyCells board)
  where
    emptyCells :: Board -> [(Int, Int)]
    emptyCells b = [(r, c) | r <- [0..8], c <- [0..8], b !! r !! c == 0]

    solve :: [(Int, Int)] -> Maybe Board
    solve [] = Just board  -- All cells filled, puzzle solved!
    solve ((r, c):rest) =
      case [num | num <- [1..9], isValid board num (r, c)] of
        [] -> Nothing  -- No valid number can be placed, backtrack
        nums ->
          -- Try each valid number in the cell and continue with the next empty cell
          case filter (\x -> solve ((r, c):rest) /= Nothing) nums of
            [] -> Nothing  -- No solution found, backtrack
            (x:_) -> solve ((r, c):rest)  -- Found a valid solution, continue

-- Example Sudoku puzzle (0 represents an empty cell).
exampleSudoku :: Board
exampleSudoku =
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

main :: IO ()
main = do
  putStrLn "Sudoku Puzzle:"
  printBoard exampleSudoku
  putStrLn "\nSolving..."
  case solveSudoku exampleSudoku of
    Just solution -> do
      putStrLn "Solution:"
      printBoard solution
    Nothing -> putStrLn "No solution found."
