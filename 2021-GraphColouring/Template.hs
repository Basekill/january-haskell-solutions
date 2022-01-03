module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

import Debug.Trace

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
buildArgAssignments [] _
  = [] 
buildArgAssignments (x : xs) idMap 
  = (Assign (lookUp x idMap) (Var x)) : (buildArgAssignments xs idMap)

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var x) idMap 
  = Var (lookUp x idMap)
renameExp (Apply op e e') idMap
  = Apply op (renameExp e idMap) (renameExp e' idMap) 
renameExp e _
  = e

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _
  = [] 
renameBlock ((Assign id e) : ss) idMap
  | Var id' == e' = renameBlock ss idMap
  | otherwise     = (Assign id' e') : (renameBlock ss idMap)
  where
    Var id' = renameExp (Var id) idMap
    e'      = renameExp e idMap
renameBlock ((If e b b') : ss) idMap
  = (If (renameExp e idMap) (renameBlock b idMap) (renameBlock b' idMap)) : (renameBlock ss idMap)
renameBlock ((While e b) : ss) idMap
  = (While (renameExp e idMap) (renameBlock b idMap)) : (renameBlock ss idMap)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG 
buildIG [] 
  = ([] , [])
buildIG vss 
  = (removeDuplicates vss, nub2 (buildIG' vss))
    where
      buildIG' :: [[Id]] -> [Edge Id]
      buildIG' []
        = []
      buildIG' (vs : vss)
        = [(v, v') | v <- vs, v' <- vs, v /= v'] ++ (buildIG' vss)
      removeDuplicates :: Eq a => [[a]] -> [a]
      removeDuplicates []
        = []
      removeDuplicates (xs : xss)
        = nub ((nub xs) ++ removeDuplicates xss)
      nub2 :: Eq a => [(a, a)] -> [(a, a)]
      nub2 []
        = []
      nub2 (p : ps)
        | elem p ps || elem (snd p, fst p) ps = nub2 ps
        | otherwise                           = p : (nub2 ps)
      

      

    

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars []
  = [] 
liveVars (((def, use), succ) : cfg)
  = use ++ [liveVars' n cfg | n <- succ] \ def
    where
      liveVars' :: Int -> CFG -> [Id]
      liveVars' n cfg
        = usen ++ [liveVars' n' | n' <- succ] \ defn 
          where
            ((defn, usen), succn) = cfg !! n


buildCFG :: Function -> CFG
buildCFG 
  = undefined