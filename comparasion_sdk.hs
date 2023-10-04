-- imports
import Data.List (nub, union, intersect)

-- definicao dos tipos de dados
type Puzzle = [[Int]]

type PossibilityTable = [[[Int]]]

type Constraint = (Int, Int, Int) 

type Position = (Int, Int)

{-     Funcoes que definem o puzzle     -}
-- geracao do puzzle (tabuleiro)
board :: Puzzle
board = 
  [ [0, 0, 0, 4, 0, 0, 0, 8, 0]
  , [3, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 8, 0, 1, 0, 6, 0, 0]
  , [0, 0, 0, 0, 0, 7, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 2, 0, 0, 0]
  , [0, 0, 6, 0, 8, 0, 3, 0, 0]
  , [8, 3, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 2, 0, 0, 0, 1, 0]
  ]

-- geracao do puzzle (restricoes)
constraints :: [Constraint]
constraints = [(0,1,0), (1,2,1), (3,4,1), (4,5,0), (6,7,0), (7,8,1)
            , (0,9,1), (1,10,1), (2,11,1), (3,12,0), (4,13,0), (5,14,0), (6,15,1), (7,16,1), (8,17,0)
            , (9,10,1), (10,11,1), (12,13,1), (13,14,0), (15,16,0), (16,17,0)
            , (9,18,0), (10,19,0), (11,20,0), (12,21,0), (13,22,1), (14, 23,1), (15,24,0), (16,25,1), (17,26,1)
            , (18,19,0), (19,20,0), (21,22,1), (22,23,0), (24,25,1), (25,26,1)
            , (27,28, 0), (28,29,1), (30,31,0), (31,32,1), (33,34,0), (34,35,0)
            , (27,36,0), (28,37,0), (29,38,0), (30,39,1), (31,40,1), (32,41,1), (33,42,0), (34,43,0), (35,43,1)
            , (36,37,1), (37,38,1), (39,40,1), (40,41,1), (42,43,0), (43,44,1)
            , (36,45,1), (37,46,1), (38,47,0), (39,48,1), (40,49,1), (41,50,0), (42,51,0), (43,52,1), (44,53,0)
            , (45,46,1), (46,47,0), (48,49,0), (49,50,1), (51,52,1), (52,53,0)
            , (54,55,0), (55,56,0), (57,58,0), (58,59,0), (60,61,1), (61,62,0)
            , (54,63,0), (55,64,1), (56,65,1), (57,66,1), (58,67,1), (59,68,1), (60,69,0), (61,70,0), (62,71,1)
            , (63,64,1), (64,65,1), (66,67,0), (67,68,1), (69,70,0), (70,71,1)
            , (63,72,1), (64,73,0), (65,74,0), (66,75,0), (67,76,1), (68,77,0), (69,78,0), (70,79,1), (71,80,1)
            , (72,73,1), (73,74,0), (75,76,0), (76,77,0), (78,79,1), (79,80,0)
            ]

{-    Funcoes que lidam com as possibilidades    -}
-- gera as possibilidades baseando se no tabuleiro fornecido
possibilities :: Puzzle -> PossibilityTable
possibilities input = map (map (\n -> if n == 0 then [1..9] else [n])) input

-- recupera as possibilidades de um elemento
getPossibilities :: (Int, Int) -> PossibilityTable -> [Int]
getPossibilities (i, j) possibilities_matrix = possibilities_matrix !! i !! j

-- atualiza a matriz de possibilidades
updatePossibilities :: (Int, Int) -> [Int] -> PossibilityTable -> PossibilityTable
updatePossibilities (i, j) new_list possibilities_matrix =
    take i possibilities_matrix ++ [updateRow (possibilities_matrix !! i) j new_list] ++ drop (i + 1) possibilities_matrix
  where
    updateRow :: [[Int]] -> Int -> [Int] -> [[Int]]
    updateRow row row_index new_values = take row_index row ++ [new_values] ++ drop (row_index + 1) row

-- revisa a consistencia entre x e y
revisionStep :: [Int] -> [Int] -> Int -> [Int]
revisionStep _ [] _ = []
revisionStep x (a:b) s | (s == -1) = a:b
                       | (s == 0) = union (keepLowerThan x a) (revisionStep x b s)
                       | otherwise = union (keepGreaterThan x a) (revisionStep x b s)
    where
        -- retira possibilidades caso o elemnto seja menor que n
        keepLowerThan :: [Int] -> Int -> [Int]
        keepLowerThan [] n = []
        keepLowerThan (a:b) n =
            if a < n then
                [a] ++ keepLowerThan b n
            else
                keepLowerThan b n

        -- retira possibilidades caso o elemnto seja maior que n
        keepGreaterThan :: [Int] -> Int -> [Int]
        keepGreaterThan [] n = []
        keepGreaterThan (a:b) n =
            if a > n then
                [a] ++ keepGreaterThan b n
            else
                keepGreaterThan b n

{-     Funcoes auxiliares do backtrack propriamente dito     -}
-- escolhe um elemento (o primeiro)
choice :: [Int] -> Int
choice l = head l

-- lineariza uma posicao
linearPosition :: Position -> Int
linearPosition (i, j) = ((i * 9) + j)

-- retorna os valores de uma linha de matriz
getRow :: [[a]] -> Int -> [a]
getRow a row = a !! row

-- retorna as posicoes de uma linha do tabuleiro (9x9)
getRowPositions :: Int -> [Position]
getRowPositions i = [(i, j) | j <- [0..8]]

-- retorna os valores de uma coluna de matriz
getColumn :: [[a]] -> Int -> [a]
getColumn a col = map (!! col) a

-- retorna as posicoes de uma coluna do tabuleiro (9x9)
getColumnPositions :: Int -> [Position]
getColumnPositions j = [(i, j) | i <- [0..8]]

-- retorna as posicoes de um grid 3x3 do tabuleiro
getSubgridPositions :: Position -> [Position]
getSubgridPositions (i, j) = [(x,y) | x <- helper i, y <- helper j]
    where
        helper n = [n - n `mod` 3 .. n - n `mod` 3 + 2]

-- retorna uma lista contendo todas as posicoes do tabuleiro
positions :: [Position]
positions = [(i, j) | i <- [0..8], j <- [0..8]]

-- retorna um inteiro que indica a comparacao entre dois valores do tabuleiro
getConstraint :: [Constraint] -> Int -> Int -> Int
getConstraint [] _ _ = -1
getConstraint ((c,d,comp):b) x y | (c == x && d == y) = comp
                                 | (c == y && d == x && comp == 1) = 0
                                 | (c == y && d == x && comp == 0) = 1
                                 | otherwise = getConstraint b x y

-- operacao de interseccao entre todas as listas da matriz
intersectionOfLists :: Eq a => [[a]] -> [a]
intersectionOfLists [] = []
intersectionOfLists lists = foldl1 intersect lists

-- mapeia a um passo de revisao para todos os vizinhos de uma celula x
mapRevision :: Position -> [Position] -> PossibilityTable -> [[Int]]
mapRevision (i,j) a p = map helper a
    where  
        helper = \x -> revisionStep (getPossibilities (i,j) p) (getPossibilities x p) (getConstraint (constraints) (linearPosition (i,j)) (linearPosition x))

-- garante a consistencia entre as possibilidades para as celulas
arcConsistency :: PossibilityTable -> PossibilityTable
arcConsistency p = ensureConsistency positions p 
    where
        ensureConsistency :: [Position] -> PossibilityTable -> PossibilityTable
        ensureConsistency [] p = p
        ensureConsistency (a:b) p =
            if (getPossibilities a p) == (getPossibilities a (updatePossibilities a (revise a) p)) then -- se houve alteracao nas possibilidades da posicao a
                ensureConsistency (b ++ [a]) (updatePossibilities a (revise a) p) -- chamada recursiva com a ao fim da fila
            else
                ensureConsistency b p -- se nao chamada recursiva retirando a da fila

        revise :: Position -> [Int]
        revise (i,j) = intersectionOfLists (mapRevision (i,j) (positions) p)

-- checa se ha ao menos uma possibilidade para cada celula (corretude da soluscao)
isValid :: PossibilityTable -> Bool
isValid p = 
    all(\row -> (checkRow (p !! row))) [0..8]
    where checkRow :: [[Int]] -> Bool
          checkRow p_list = all(\x -> not (null (p_list !! x))) [0..8]

-- checa completude da solucao <TODO>
isDone :: PossibilityTable -> Bool
isDone p = 
    all(\row -> (checkRow (p !! row))) [0..8]
    where checkRow :: [[Int]] -> Bool
          checkRow p_list = all(\x -> (length (p_list !! x)) == 1) [0..8]

-- Backtracking function to solve the Sudoku puzzle
solveSudoku :: Puzzle -> Maybe Puzzle
solveSudoku puzzle = solve (possibilities puzzle)
  where
    solve :: PossibilityTable -> Maybe Puzzle
    solve p
      | not (isValid p) = Nothing  -- Check for correctness
      | isDone p = Just (map (map head) p)  -- Check if puzzle is complete
      | otherwise = tryCandidates p
      where
        tryCandidates :: PossibilityTable -> Maybe Puzzle
        tryCandidates p' = 
          case findEmptyPosition p' of
            Nothing -> Nothing
            Just (i, j) -> 
              case getPossibilities (i, j) p' of
                [] -> Nothing
                (candidate:rest) -> 
                  case solve (updatePossibilities (i, j) [candidate] p') of
                    Nothing -> tryCandidates (updatePossibilities (i, j) rest p')
                    Just solution -> Just solution

        findEmptyPosition :: PossibilityTable -> Maybe Position
        findEmptyPosition p'' =
          case [ (i, j) | i <- [0..8], j <- [0..8], null (getPossibilities (i, j) p'') ] of
            [] -> Nothing
            (pos:_) -> Just pos

main = do
    --print (revisionStep (getPossibilities (1,4) (possibilities (board))) (getPossibilities (0,4) (possibilities (board))) (getConstraint (constraints) (linearPosition (1,4)) (linearPosition (0,4))))
    --print (mapRevision (4,0) (positions) (possibilities (board)))
    print (solveSudoku (board))
    --print (possibilities (board))