-- definicao dos tipos de dados
type Puzzle = [[Int]]

type Possibilities = [Int]

type PossibilityTable = [[Possibilities]]

type Constraint = (Int, Int, Int) 

type Position = (Int, Int)

allDiffConstraints :: (Int -> [Constraint]) -> [Constraint]
allDiffConstraints f = helper 0 9
    where
        helper :: Int -> Int -> [Constraint]
        helper a b = 
            if a == b then
                []
            else
                (f a) ++ helper (a+1) b

columnConstraints :: Int -> [(Int,Int,Int)]
columnConstraints a = [((x*9)+a,(y*9)+a,2) | x <- [0..8], y <- [(x+1)..8]]

rowConstraints :: Int -> [Constraint]
rowConstraints a = [(x,(a*9)+y,2) | x <- [(a*9)..(a*9)+8], y <- [(mod x 9)+1..8]]

gridConstraints :: Int -> [Constraint]
gridConstraints a = []

c :: [Constraint]
c = ([(0,1,2),(0,2,2),(0,3,2),(0,4,2),(0,5,2),(0,6,2),(0,7,2),(0,8,2),(1,2,2)
    ,(1,3,2),(1,4,2),(1,5,2),(1,6,2),(1,7,2),(1,8,2),(2,3,2),(2,4,2),(2,5,2)
    ,(2,6,2),(2,7,2),(2,8,2),(3,4,2),(3,5,2),(3,6,2),(3,7,2),(3,8,2),(4,5,2)
    ,(4,6,2),(4,7,2),(4,8,2),(5,6,2),(5,7,2),(5,8,2),(6,7,2),(6,8,2),(7,8,2)
    ,(9,10,2),(9,11,2),(9,12,2),(9,13,2),(9,14,2),(9,15,2),(9,16,2),(9,17,2),(10,11,2)
    ,(10,12,2),(10,13,2),(10,14,2),(10,15,2),(10,16,2),(10,17,2),(11,12,2),(11,13,2),(11,14,2)
    ,(11,15,2),(11,16,2),(11,17,2),(12,13,2),(12,14,2),(12,15,2),(12,16,2),(12,17,2),(13,14,2)
    ,(13,15,2),(13,16,2),(13,17,2),(14,15,2),(14,16,2),(14,17,2),(15,16,2),(15,17,2),(16,17,2)
    ,(18,19,2),(18,20,2),(18,21,2),(18,22,2),(18,23,2),(18,24,2),(18,25,2),(18,26,2),(19,20,2)
    ,(19,21,2),(19,22,2),(19,23,2),(19,24,2),(19,25,2),(19,26,2),(20,21,2),(20,22,2),(20,23,2)
    ,(20,24,2),(20,25,2),(20,26,2),(21,22,2),(21,23,2),(21,24,2),(21,25,2),(21,26,2),(22,23,2)
    ,(22,24,2),(22,25,2),(22,26,2),(23,24,2),(23,25,2),(23,26,2),(24,25,2),(24,26,2),(25,26,2)
    ,(27,28,2),(27,29,2),(27,30,2),(27,31,2),(27,32,2),(27,33,2),(27,34,2),(27,35,2),(28,29,2)
    ,(28,30,2),(28,31,2),(28,32,2),(28,33,2),(28,34,2),(28,35,2),(29,30,2),(29,31,2),(29,32,2)
    ,(29,33,2),(29,34,2),(29,35,2),(30,31,2),(30,32,2),(30,33,2),(30,34,2),(30,35,2),(31,32,2)
    ,(31,33,2),(31,34,2),(31,35,2),(32,33,2),(32,34,2),(32,35,2),(33,34,2),(33,35,2),(34,35,2)
    ,(36,37,2),(36,38,2),(36,39,2),(36,40,2),(36,41,2),(36,42,2),(36,43,2),(36,44,2),(37,38,2)
    ,(37,39,2),(37,40,2),(37,41,2),(37,42,2),(37,43,2),(37,44,2),(38,39,2),(38,40,2),(38,41,2)
    ,(38,42,2),(38,43,2),(38,44,2),(39,40,2),(39,41,2),(39,42,2),(39,43,2),(39,44,2),(40,41,2)
    ,(40,42,2),(40,43,2),(40,44,2),(41,42,2),(41,43,2),(41,44,2),(42,43,2),(42,44,2),(43,44,2)
    ,(45,46,2),(45,47,2),(45,48,2),(45,49,2),(45,50,2),(45,51,2),(45,52,2),(45,53,2),(46,47,2)
    ,(46,48,2),(46,49,2),(46,50,2),(46,51,2),(46,52,2),(46,53,2),(47,48,2),(47,49,2),(47,50,2)
    ,(47,51,2),(47,52,2),(47,53,2),(48,49,2),(48,50,2),(48,51,2),(48,52,2),(48,53,2),(49,50,2)
    ,(49,51,2),(49,52,2),(49,53,2),(50,51,2),(50,52,2),(50,53,2),(51,52,2),(51,53,2),(52,53,2)
    ,(54,55,2),(54,56,2),(54,57,2),(54,58,2),(54,59,2),(54,60,2),(54,61,2),(54,62,2),(55,56,2)
    ,(55,57,2),(55,58,2),(55,59,2),(55,60,2),(55,61,2),(55,62,2),(56,57,2),(56,58,2),(56,59,2)
    ,(56,60,2),(56,61,2),(56,62,2),(57,58,2),(57,59,2),(57,60,2),(57,61,2),(57,62,2),(58,59,2)
    ,(58,60,2),(58,61,2),(58,62,2),(59,60,2),(59,61,2),(59,62,2),(60,61,2),(60,62,2),(61,62,2)
    ,(63,64,2),(63,65,2),(63,66,2),(63,67,2),(63,68,2),(63,69,2),(63,70,2),(63,71,2),(64,65,2)
    ,(64,66,2),(64,67,2),(64,68,2),(64,69,2),(64,70,2),(64,71,2),(65,66,2),(65,67,2),(65,68,2)
    ,(65,69,2),(65,70,2),(65,71,2),(66,67,2),(66,68,2),(66,69,2),(66,70,2),(66,71,2),(67,68,2)
    ,(67,69,2),(67,70,2),(67,71,2),(68,69,2),(68,70,2),(68,71,2),(69,70,2),(69,71,2),(70,71,2)
    ,(72,73,2),(72,74,2),(72,75,2),(72,76,2),(72,77,2),(72,78,2),(72,79,2),(72,80,2),(73,74,2)
    ,(73,75,2),(73,76,2),(73,77,2),(73,78,2),(73,79,2),(73,80,2),(74,75,2),(74,76,2),(74,77,2)
    ,(74,78,2),(74,79,2),(74,80,2),(75,76,2),(75,77,2),(75,78,2),(75,79,2),(75,80,2),(76,77,2)
    ,(76,78,2),(76,79,2),(76,80,2),(77,78,2),(77,79,2),(77,80,2),(78,79,2),(78,80,2),(79,80,2)]

    ++

    [(0,9,2),(0,18,2),(0,27,2),(0,36,2),(0,45,2),(0,54,2),(0,63,2),(0,72,2),(9,18,2)
    ,(9,27,2),(9,36,2),(9,45,2),(9,54,2),(9,63,2),(9,72,2),(18,27,2),(18,36,2),(18,45,2)
    ,(18,54,2),(18,63,2),(18,72,2),(27,36,2),(27,45,2),(27,54,2),(27,63,2),(27,72,2),(36,45,2)
    ,(36,54,2),(36,63,2),(36,72,2),(45,54,2),(45,63,2),(45,72,2),(54,63,2),(54,72,2),(63,72,2)
    ,(1,10,2),(1,19,2),(1,28,2),(1,37,2),(1,46,2),(1,55,2),(1,64,2),(1,73,2),(10,19,2)
    ,(10,28,2),(10,37,2),(10,46,2),(10,55,2),(10,64,2),(10,73,2),(19,28,2),(19,37,2),(19,46,2)
    ,(19,55,2),(19,64,2),(19,73,2),(28,37,2),(28,46,2),(28,55,2),(28,64,2),(28,73,2),(37,46,2)
    ,(37,55,2),(37,64,2),(37,73,2),(46,55,2),(46,64,2),(46,73,2),(55,64,2),(55,73,2),(64,73,2)
    ,(2,11,2),(2,20,2),(2,29,2),(2,38,2),(2,47,2),(2,56,2),(2,65,2),(2,74,2),(11,20,2)
    ,(11,29,2),(11,38,2),(11,47,2),(11,56,2),(11,65,2),(11,74,2),(20,29,2),(20,38,2),(20,47,2)
    ,(20,56,2),(20,65,2),(20,74,2),(29,38,2),(29,47,2),(29,56,2),(29,65,2),(29,74,2),(38,47,2)
    ,(38,56,2),(38,65,2),(38,74,2),(47,56,2),(47,65,2),(47,74,2),(56,65,2),(56,74,2),(65,74,2)
    ,(3,12,2),(3,21,2),(3,30,2),(3,39,2),(3,48,2),(3,57,2),(3,66,2),(3,75,2),(12,21,2)
    ,(12,30,2),(12,39,2),(12,48,2),(12,57,2),(12,66,2),(12,75,2),(21,30,2),(21,39,2),(21,48,2)
    ,(21,57,2),(21,66,2),(21,75,2),(30,39,2),(30,48,2),(30,57,2),(30,66,2),(30,75,2),(39,48,2)
    ,(39,57,2),(39,66,2),(39,75,2),(48,57,2),(48,66,2),(48,75,2),(57,66,2),(57,75,2),(66,75,2)
    ,(4,13,2),(4,22,2),(4,31,2),(4,40,2),(4,49,2),(4,58,2),(4,67,2),(4,76,2),(13,22,2)
    ,(13,31,2),(13,40,2),(13,49,2),(13,58,2),(13,67,2),(13,76,2),(22,31,2),(22,40,2),(22,49,2)
    ,(22,58,2),(22,67,2),(22,76,2),(31,40,2),(31,49,2),(31,58,2),(31,67,2),(31,76,2),(40,49,2)
    ,(40,58,2),(40,67,2),(40,76,2),(49,58,2),(49,67,2),(49,76,2),(58,67,2),(58,76,2),(67,76,2)
    ,(5,14,2),(5,23,2),(5,32,2),(5,41,2),(5,50,2),(5,59,2),(5,68,2),(5,77,2),(14,23,2)
    ,(14,32,2),(14,41,2),(14,50,2),(14,59,2),(14,68,2),(14,77,2),(23,32,2),(23,41,2),(23,50,2)
    ,(23,59,2),(23,68,2),(23,77,2),(32,41,2),(32,50,2),(32,59,2),(32,68,2),(32,77,2),(41,50,2)
    ,(41,59,2),(41,68,2),(41,77,2),(50,59,2),(50,68,2),(50,77,2),(59,68,2),(59,77,2),(68,77,2)
    ,(6,15,2),(6,24,2),(6,33,2),(6,42,2),(6,51,2),(6,60,2),(6,69,2),(6,78,2),(15,24,2)
    ,(15,33,2),(15,42,2),(15,51,2),(15,60,2),(15,69,2),(15,78,2),(24,33,2),(24,42,2),(24,51,2)
    ,(24,60,2),(24,69,2),(24,78,2),(33,42,2),(33,51,2),(33,60,2),(33,69,2),(33,78,2),(42,51,2)
    ,(42,60,2),(42,69,2),(42,78,2),(51,60,2),(51,69,2),(51,78,2),(60,69,2),(60,78,2),(69,78,2)
    ,(7,16,2),(7,25,2),(7,34,2),(7,43,2),(7,52,2),(7,61,2),(7,70,2),(7,79,2),(16,25,2)
    ,(16,34,2),(16,43,2),(16,52,2),(16,61,2),(16,70,2),(16,79,2),(25,34,2),(25,43,2),(25,52,2)
    ,(25,61,2),(25,70,2),(25,79,2),(34,43,2),(34,52,2),(34,61,2),(34,70,2),(34,79,2),(43,52,2)
    ,(43,61,2),(43,70,2),(43,79,2),(52,61,2),(52,70,2),(52,79,2),(61,70,2),(61,79,2),(70,79,2)
    ,(8,17,2),(8,26,2),(8,35,2),(8,44,2),(8,53,2),(8,62,2),(8,71,2),(8,80,2),(17,26,2)
    ,(17,35,2),(17,44,2),(17,53,2),(17,62,2),(17,71,2),(17,80,2),(26,35,2),(26,44,2),(26,53,2)
    ,(26,62,2),(26,71,2),(26,80,2),(35,44,2),(35,53,2),(35,62,2),(35,71,2),(35,80,2),(44,53,2)
    ,(44,62,2),(44,71,2),(44,80,2),(53,62,2),(53,71,2),(53,80,2),(62,71,2),(62,80,2),(71,80,2)])

main = do
    print ()