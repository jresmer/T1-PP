-- imports
import Data.List

-- definicao dos tipos de dados
type Puzzle = [[Int]]

type Possibilities = [Int]

type PossibilityTable = [[Possibilities]]

type Constraint = (Int, Int, Int) 

type Position = (Int, Int)

{-     Funcoes que definem o puzzle     -}
-- geracao do puzzle (tabuleiro)
board :: Puzzle
board = 
  [ [9, 0, 0, 0, 0, 0, 0, 0, 3]
  , [5, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 1, 0, 0, 0]
  , [8, 0, 0, 0, 3, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 3, 0, 0, 6, 0, 0, 0]
  , [1, 0, 0, 0, 0, 0, 0, 0, 0]
  ]


-- geracao do puzzle (restricoes)
constraints :: [Constraint]
constraints = [(0,1,1), (1,2,0), (3,4,0), (4,5,1), (6,7,0), (7,8,1)
            , (0,9,1), (1,10,0), (2,11,0), (3,12,1), (4,13,1), (5,14,0), (6,15,0), (7,16,1), (8,17,0)
            , (9,10,1), (10,11,0), (12,13,0), (13,14,0), (15,16,1), (16,17,0)
            , (9,18,0), (10,19,0), (11,20,1), (12,21,0), (13,22,0), (14, 23,1), (15,24,1), (16,25,0), (17,26,0)
            , (18,19,1), (19,20,1), (21,22,0), (22,23,1), (24,25,0), (25,26,0)
            , (27,28, 0), (28,29,1), (30,31,0), (31,32,1), (33,34,1), (34,35,0)
            , (27,36,0), (28,37,1), (29,38,1), (30,39,0), (31,40,1), (32,41,0), (33,42,1), (34,43,0), (35,44,1)
            , (36,37,1), (37,38,1), (39,40,1), (40,41,0), (42,43,0), (43,44,1)
            , (36,45,1), (37,46,1), (38,47,0), (39,48,0), (40,49,0), (41,50,0), (42,51,0), (43,52,1), (44,53,0)
            , (45,46,1), (46,47,0), (48,49,0), (49,50,1), (51,52,1), (52,53,0)
            , (54,55,0), (55,56,1), (57,58,1), (58,59,0), (60,61,1), (61,62,0)
            , (54,63,1), (55,64,0), (56,65,0), (57,66,1), (58,67,0), (59,68,0), (60,69,0), (61,70,1), (62,71,1)
            , (63,64,0), (64,65,1), (66,67,1), (67,68,0), (69,70,1), (70,71,0)
            , (63,72,1), (64,73,1), (65,74,0), (66,75,0), (67,76,1), (68,77,1), (69,78,1), (70,79,0), (71,80,0)
            , (72,73,0), (73,74,0), (75,76,1), (76,77,0), (78,79,0), (79,80,1)
            ]

{-    Funcoes que lidam com as possibilidades    -}
-- gera as possibilidades baseando se no tabuleiro fornecido
-- retorna uma lista contendo todas as posicoes do tabuleiro
positions :: [Position]
positions = [(i, j) | i <- [0..8], j <- [0..8]]

possibilities :: Puzzle -> PossibilityTable
possibilities input = map (map (\n -> if n == 0 then [1..9] else [n])) input

noPossibilities :: PossibilityTable
noPossibilities = helper (board)
    where
        helper :: Puzzle -> PossibilityTable
        helper input = map (map (\n -> [])) input

-- recupera as possibilidades de um elemento
getPossibilities :: Position -> PossibilityTable -> [Int]
getPossibilities (i, j) possibilities_matrix = possibilities_matrix !! i !! j

-- atualiza a matriz de possibilidades
updatePossibilities :: Position -> [Int] -> PossibilityTable -> PossibilityTable
updatePossibilities (i, j) new_list possibilities_matrix =
    take i possibilities_matrix ++ [updateRow (possibilities_matrix !! i) j new_list] ++ drop (i + 1) possibilities_matrix
  where
    updateRow :: [[Int]] -> Int -> [Int] -> [[Int]]
    updateRow row row_index new_values = take row_index row ++ [new_values] ++ drop (row_index + 1) row

-- revisa a consistencia entre x e y
revisionStep :: [Int] -> [Int] -> Int -> [Int]
revisionStep _ [] _ = []
revisionStep x (y:ys) s | (s == -1) = x
                       | (s == 0) = union (keepLowerThan x y) (revisionStep x ys s)
                       | (s == 1) = union (keepGreaterThan x y) (revisionStep x ys s)
                       | otherwise = union (keepDiffThan x y) (revisionStep x ys s)
    where
        -- retira possibilidades caso o elemnto seja maior que n
        keepLowerThan :: [Int] -> Int -> [Int]
        keepLowerThan [] n = []
        keepLowerThan (a:b) n =
            if a < n then
                [a] ++ keepLowerThan b n
            else
                keepLowerThan b n

        -- retira possibilidades caso o elemnto seja menor que n
        keepGreaterThan :: [Int] -> Int -> [Int]
        keepGreaterThan [] n = []
        keepGreaterThan (a:b) n =
            if a > n then
                [a] ++ keepGreaterThan b n
            else
                keepGreaterThan b n

        -- retira possibidades caso o elemento seja igualm a n
        keepDiffThan :: [Int] -> Int -> [Int]
        keepDiffThan [] _ = []
        keepDiffThan (a:b) n =
          if a == n then
              keepDiffThan b n
          else
              [a] ++ keepDiffThan b n

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

-- retorna um inteiro que indica a comparacao entre dois valores do tabuleiro
getConstraint :: [Constraint] -> Int -> Int -> Int
getConstraint [] _ _ = 2
getConstraint ((c,d,comp):b) x y | (c == x && d == y) = comp
                                 | (c == y && d == x && comp == 1) = 0
                                 | (c == y && d == x && comp == 0) = 1
                                 | otherwise = getConstraint b x y

-- operacao de interseccao entre todas as listas da matriz
intersectionOfLists :: Eq a => [[a]] -> [a]
intersectionOfLists [] = []
intersectionOfLists lists = foldl1 intersect lists

-- gera a lista de posicoes dos vizinhos
sdkNeighbors :: Position -> [Position]
sdkNeighbors pos = (union (gridNeighbors pos)  (union (rowNeighbors pos)  (columnNeighbors pos))) \\ [pos]
    where
        gridNeighbors :: Position -> [Position]
        gridNeighbors (i,j) = [(x, y) | x <- [3 * (i `div` 3) .. 3 * (i `div` 3 + 1) - 1]
                            , y <- [3 * (j `div` 3) .. 3 * (j `div` 3 + 1) - 1]]

        rowNeighbors :: Position -> [Position]
        rowNeighbors (i,j) = [(x,y) | x <- [i], y <- ([0..8] \\ [j])]

        columnNeighbors :: Position -> [Position]
        columnNeighbors (i,j) = [(x,y) | x <- ([0..8] \\ [i]), y <- [j]]

-- cria pares entre uma posição (p) e as posições que se relacionam com ela (q) no formato (p,q)
pairWithNeighbors :: Position -> [(Position,Position)]
pairWithNeighbors pos = helper pos (sdkNeighbors pos)
    where
        helper :: Position -> [Position] -> [(Position,Position)]
        helper pos [] = []
        helper pos (x:xs) = [(pos, x)] ++ helper pos xs

-- cria pares entre uma posição (p) e as posições que se relacionam com ela (q) no formato (q,p)
rPairWithNeighbors :: Position -> [(Position,Position)]
rPairWithNeighbors pos = helper pos (sdkNeighbors pos)
    where
        helper :: Position -> [Position] -> [(Position,Position)]
        helper pos [] = []
        helper pos (x:xs) = [(x, pos)] ++ helper pos xs

-- cria uma lista com todos os pares de posições relacionadas do tabuleiro
allPairs :: [(Position,Position)]
allPairs = helper (positions)
    where
        helper :: [Position] -> [(Position,Position)]
        helper [] = []
        helper (x:xs) = (pairWithNeighbors x) ++ helper xs

{-

Algoritmos para o problema de satisfacao de restricoes

AC(csp): --> arcConsistency
  queue = all arcs in csp
  while queue non-empty: --> base da recursao de ensureConsistency
    (X, Y) = dequeue(queue)
    if revise(csp, X, Y):
      if size of X.domain == 0: --> primeiro caso de ensureConsistency
        return false
      for each Z in X.neighbors - {Y}: --> ultimo caso de ensureConsitency
        enqueue(queue, (Z, X))

revise(csp, X, Y): 
  revised = false
  for x in X.domain: --> revisionStep
    if no y in Y.domain satifies constraint for (X, Y):
      delete x from X.domain
      revised = true
  return revised

-}

-- garante a consistencia entre as possibilidades para as celulas
arcConsistency :: PossibilityTable -> PossibilityTable
arcConsistency p = ensureConsistency allPairs p
    where
        ensureConsistency :: [(Position,Position)] -> PossibilityTable -> PossibilityTable
        ensureConsistency [] p = p
        ensureConsistency ((pos1,pos2):xs) p | null (revisionStep (getPossibilities pos1 p) (getPossibilities pos2 p) (getConstraint (constraints) (linearPosition pos1) (linearPosition pos2))) = (updatePossibilities pos1 (revisionStep (getPossibilities pos1 p) (getPossibilities pos2 p) (getConstraint (constraints) (linearPosition pos1) (linearPosition pos2))) p)
                                             | null ((getPossibilities pos1 p) \\ (revisionStep (getPossibilities pos1 p) (getPossibilities pos2 p) (getConstraint (constraints) (linearPosition pos1) (linearPosition pos2)))) = ensureConsistency xs p
                                             | otherwise = ensureConsistency  (union xs ((rPairWithNeighbors pos1) \\ [(pos2,pos1)])) (updatePossibilities pos1 (revisionStep (getPossibilities pos1 p) (getPossibilities pos2 p) (getConstraint (constraints) (linearPosition pos1) (linearPosition pos2))) p)

-- checa se ha ao menos uma possibilidade para cada celula (corretude da soluscao)
isValid :: PossibilityTable -> Bool
isValid p = 
    all(\row -> (checkRow (p !! row))) [0..8]
    where checkRow :: [[Int]] -> Bool
          checkRow p_list = all(\x -> not (null (p_list !! x))) [0..8]

-- checa completude da solucao
isDone :: PossibilityTable -> Bool
isDone p =
    all(\row -> (checkRow (p !! row))) [0..8]
    where checkRow :: [[Int]] -> Bool
          checkRow p_list = all(\x -> (length (p_list !! x)) == 1) [0..8]

-- Backtracking function to solve the Sudoku puzzle
backtrack :: PossibilityTable -> [Constraint] -> Maybe Puzzle
backtrack possibilities constraints
    | not (isValid possibilities) = Nothing
    | isDone (arcConsistency possibilities) = Just (extractSolution (arcConsistency possibilities))
    | otherwise = tryChoices (minimumRemainingPossibilities possibilities) possibilities
  where
    -- tente todas as possibilidades para a célula com menos possibilidades restantes
    tryChoices ((position, choices):rest) currentPossibilities =
        case tryChoice position choices of
            Just newPossibilities -> case (backtrack newPossibilities constraints) of -- chamada recursiva da backtracking
                    Nothing -> tryChoices rest currentPossibilities -- tenta outras possibilidades
                    Just solution -> Just solution
            Nothing -> tryChoices rest currentPossibilities

    tryChoice position (choice:choices) =
        let updatedPossibilities = arcConsistency (updatePossibilities position [choice] possibilities)
        in if isValid updatedPossibilities
            then Just updatedPossibilities
            else tryChoice position choices

    -- transforma possibilityTable em Puzzle
    extractSolution :: PossibilityTable -> Puzzle
    extractSolution possTable = [[head (possibilities !! i !! j) | j <- [0..8]] | i <- [0..8]]

    -- cria uma lista de posicões ordenada de forma crescente pelo número de possibilidades restantes
    minimumRemainingPossibilities :: PossibilityTable -> [(Position, [Int])]
    minimumRemainingPossibilities possTable =
        let filteredPossibilities = filter (\(_, choices) -> (length choices) > 1) [(pos, getPossibilities pos possTable) | pos <- positions]
        in sortOn (\(_, choices) -> length choices) filteredPossibilities

main :: IO ()
main = do
    let solvedPuzzle = backtrack (arcConsistency (possibilities board)) constraints
    case solvedPuzzle of
        Just solution -> putStrLn "Solution found:" >> print solution
        Nothing -> putStrLn "No solution found."
