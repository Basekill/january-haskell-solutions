module SOL where

import Data.List
import Data.Maybe
import Debug.Trace

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

-- Total: 25/25 (100%) (26/27 including bonus marks)
--------------------------------------------------------------------------
-- Part I: 5/5

-- 1/1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp k t
  = (snd . head) (filter ((==k) . fst) t)

-- 3/3 marks
vars :: Formula -> [Id]
vars (Var i)
  = [i] 
vars (Not f)
  = vars f
vars (And f f')
  = (sort . nub) (vars f ++ vars f')
vars (Or f f')
  = (sort . nub) (vars f ++ vars f')

-- 1/1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..] 

--------------------------------------------------------------------------
-- Part II: 10/11

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 3/4 marks: Missed toNNF for the Not (Not f) case
-- toNNF :: Formula -> NNF
-- toNNF (Not (Or f f'))
--   = And (toNNF (Not f)) (toNNF (Not f'))
-- toNNF (Not (And f f'))
--   = Or (toNNF (Not f)) (toNNF (Not f'))
-- toNNF (Not (Not f))
--   = f
-- toNNF (Not f)
--   = Not (toNNF f)
-- toNNF (And f f')
--   = And (toNNF f) (toNNF f')
-- toNNF (Or f f')
--   = Or (toNNF f) (toNNF f')
-- toNNF f
--   = f

-- * Adds toNNF for Not (Not f) case
toNNF :: Formula -> NNF
toNNF (Not (Or f f'))
  = And (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (And f f'))
  = Or (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Not f))
  = toNNF f
toNNF (Not f)
  = Not (toNNF f)
toNNF (And f f')
  = And (toNNF f) (toNNF f')
toNNF (Or f f')
  = Or (toNNF f) (toNNF f')
toNNF f
  = f


-- 3/3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' (toNNF f)
    where
      toCNF' :: NNF -> CNF
      toCNF' (And f f')
        = And (toCNF' f) (toCNF' f')
      toCNF' (Or f f')
        = distribute (toCNF' f) (toCNF' f')
      toCNF' f
        = f

-- 4/4 marks
-- flatten :: CNF -> CNFRep
-- flatten cnf 
--   = flatten' cnf 
--     where
--       ids = idMap cnf
--       flattenClause :: Formula -> [Int]
--       -- Pre: Given formula must be a clause
--       flattenClause (Or f f')
--         = (flattenClause f) ++ (flattenClause f')
--       flattenClause (Var i)
--         = [lookUp i ids]
--       flattenClause (Not (Var i))
--         = [-lookUp i ids]
--       flatten' :: CNF -> CNFRep
--       flatten' (And f f')
--         = flatten' f ++ flatten' f'
--       flatten' f
--         = [flattenClause f]

-- * No need for extra flattenClause helper function
flatten :: CNF -> CNFRep
flatten cnf 
  = flatten' cnf 
    where
      ids = idMap cnf
      flatten' :: CNF -> CNFRep
      flatten' (And f f')
        = flatten' f ++ flatten' f'
      flatten' (Or f f')
        = [concat ((flatten' f) ++ (flatten' f'))]
      flatten' (Var i)
        = [[lookUp i ids]]
      flatten' (Not (Var i))
        = [[-lookUp i ids]]
         
--------------------------------------------------------------------------
-- Part III: 9/9

-- 5/5 marks
-- propUnits :: CNFRep -> (CNFRep, [Int])
-- propUnits []
--   = ([], [])
-- propUnits f
--   | isNothing u = (f, [])
--   | otherwise   = (f', u' : us)
--     where
--       u         = findSingle f
--       u'        = fromJust u
--       (f', us) = propUnits 
--                  (map (filter (/=(-u'))) (filter (not . elem u') f))
--       findSingle :: CNFRep -> Maybe Int
--       findSingle ((_ : _ : _) : f)
--         = findSingle f
--       findSingle ((u : _) : f)
--         = Just u
--       findSingle ([] : f)
--         = findSingle f
--       findSingle x
--         = Nothing

-- * Using pattern guards for findSingle 
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits []
  = ([], [])
propUnits f
  | isNothing u = (f, [])
  | otherwise   = (f', u' : us)
    where
      u         = findSingle f
      u'        = fromJust u
      (f', us) = propUnits 
                 (map (filter (/=(-u'))) (filter (not . elem u') f))
      findSingle :: CNFRep -> Maybe Int
      findSingle f
        | [u] : cs <- f = Just u
        | _ : cs   <- f = findSingle cs
        | otherwise     = Nothing

-- 4/4 marks
dp :: CNFRep -> [[Int]]
dp f
  | []              <- f' = [us]
  | ((x : xs) : cs) <- f' = (map (us++) (dp ([x] : f'))) ++ 
                            (map (us++) (dp ([-x] : f')))
  | otherwise = []
    where
      (f', us) = propUnits f

--------------------------------------------------------------------------
-- Part IV: 2/2

lookUp' :: Eq b => [(a, b)] -> b -> a
-- Pre: The item being looked up has a unique binding in the list
lookUp' t k
  = (fst . head) (filter ((==k) . snd) t)

-- Bonus 2/2 marks
-- allSat :: Formula -> [[(Id, Bool)]]
-- allSat f
--   = allSat' sols
--     where
--       sols = (dp . flatten . toCNF) f
--       allSat' :: [[Int]] -> [[(Id, Bool)]]
--       allSat' []
--         = []
--       allSat' (sol : sols)
--         = (addMissing ms as) ++ allSat' sols
--         where
--           ts   = map (>0) sol
--           vs   = map ((lookUp' (idMap f)) . abs) sol
--           as   = zip vs ts
--           ms   = (vars f) \\ vs
--           addMissing :: [Id] -> [(Id, Bool)] -> [[(Id, Bool)]]
--           addMissing [] as
--             = [as]
--           addMissing (m : ms) as
--             = addMissing ms (sort ((m, True) : as)) ++ addMissing ms (sort ((m, False) : as)) -- * sort added afterwards

-- * Alternative
lookUp2 :: Eq b => b -> [(a, b)] -> a
-- Pre: The item being looked up has a unique binding in the list
lookUp2 k t
  = (fst . head) (filter ((==k) . snd) t)
      
allSat :: Formula -> [[(Id, Bool)]]
allSat f
  = allSat' sols
    where
      sols = (dp . flatten . toCNF) f
      allSat' :: [[Int]] -> [[(Id, Bool)]]
      allSat' sols
        = concat (zipWith addMissing mss ass)
          where
            ass = map toAs sols
            mss = map (\as -> (vars f) \\ (map fst as)) ass
            toAs :: [Int] -> [(Id, Bool)]
            toAs
              = sort . map (\n -> (lookUp2 (abs n) (idMap f), n > 0))
            addMissing :: [Id] -> [(Id, Bool)] -> [[(Id, Bool)]]
            addMissing [] as
              = [as]
            addMissing (m : ms) as
              = addMissing ms (sort ((m, True) : as)) ++ addMissing ms (sort ((m, False) : as))

-- allSat :: Formula -> [[(Id, Bool)]]
-- allSat f
--   = allSat' sols
--     where
--       sols = (dp . flatten . toCNF) f
--       allSat' :: [[Int]] -> [[(Id, Bool)]]
--       allSat' []
--         = []
--       allSat' (sol : sols)
--         = (addMissing ms as) ++ allSat' sols
--         where
--           ts   = (map . map (>0)) sols
--           vs   = (map . map ((lookUp' (idMap f)) . abs)) sols
--           as   = [zip t v | (t <- ts, v <- vs] 
--           ms   = map ((vars f) \\) vs
--           addMissing :: [[Id]] -> [[(Id, Bool)]] -> [[(Id, Bool)]]
--           addMissing [] as
--             = as
--           addMissing (m : ms) as
--             = addMissing ms ((m, True) : as) ++ addMissing ms ((m, False) : as)
-- 