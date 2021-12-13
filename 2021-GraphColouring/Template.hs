module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
-- count :: Eq a => a -> [a] -> Int
-- count _ []
--   = 0
-- count a (x : xs)
--   | a == x    = 1 + count a xs
--   | otherwise = count a xs

count :: Eq a => a -> [a] -> Int
count x
  = length . filter (==x)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = [(n, count n (map fst es) + count n (map snd es)) | n <- ns]

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (_, es)
  = [x | (x, y) <- es, n == y] ++ [y | (x, y) <- es, n == x]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (filter (/=n) ns, filter (\x -> (fst x) /= n && (snd x) /= n) es)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph maxC g
  | null unusedC = (n, 0) : cMap 
  | otherwise = (n, minimum unusedC) : cMap
  where
    n = snd $ minimum (map (\(x, y) -> (y, x)) (degrees g))
    g'      = removeNode n g
    cMap = colourGraph maxC g'
    usedC = [lookUp neighbour cMap | neighbour <- neighbours n g]
    unusedC = [1..maxC] \\ usedC



------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap []
  = [("return", "return")]
buildIdMap ((v, c) : cs)
  | c == 0    = (v, v) : (buildIdMap cs)
  | otherwise = (v, 'R' : show c) : (buildIdMap cs)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments 
  = undefined

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp 
  = undefined

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock 
  = undefined

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined