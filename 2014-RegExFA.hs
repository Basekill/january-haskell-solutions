import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I: 3/4

-- 1/1
lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp k t
  = head [y | (x, y) <- t, x == k] 

-- 2/3: Missing simplify for Plus and Opt
-- simplify :: RE -> RE
-- simplify (Seq re re')
--   = Seq (simplify re) (simplify re')
-- simplify (Alt re re')
--   = Alt (simplify re) (simplify re')
-- simplify (Rep re)
--   = Rep (simplify re)
-- simplify (Plus re)
--   = Seq re (Rep re)
-- simplify (Opt re)
--   = Alt re Null
-- simplify re
--   = re

-- * Remember to apply the function recursively to all re arguments!
simplify :: RE -> RE
simplify (Seq re re')
  = Seq (simplify re) (simplify re')
simplify (Alt re re')
  = Alt (simplify re) (simplify re')
simplify (Rep re)
  = Rep (simplify re)
simplify (Plus re)
  = Seq sre (Rep sre)
  where
    sre = simplify re
simplify (Opt re)
  = Alt (simplify re) Null
simplify re
  = re


--------------------------------------------------------
-- Part II: 10/11

-- 1/1
startState :: Automaton -> State
startState (s, _, _)
  = s 
terminalStates :: Automaton -> [State]
terminalStates (_, tes, _)
  = tes
transitions :: Automaton -> [Transition]
transitions (_, _, ts)
  = ts 

-- 1/1
isTerminal :: State -> Automaton -> Bool
isTerminal s a
  = elem s (terminalStates a) 

-- 2/2
transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a
  = filter (\(from, _, _) -> from == s) (transitions a)

-- 1/1
labels :: [Transition] -> [Label]
labels ts
  = nub [lb | (_, _, lb) <- ts, lb /= Eps]

-- 5/6: Guards could be simplified
-- accepts :: Automaton -> String -> Bool
-- accepts a str
--   = accepts' (startState a) str
--     where
--       accepts' :: State -> String -> Bool
--       accepts' s str
--         | isTerminal s a && null str = True
--         | otherwise = any (try str) (transitionsFrom s a)
--           where
--             try :: String -> Transition -> Bool
--             try str (_, t, Eps)
--               = accepts' t str
--             try "" _ 
--               = False
--             try (c' : str) (_, t, C c)
--               | c' == c   = accepts' t str
--               | otherwise = False

-- * Using extensionality and simplifying guards
accepts :: Automaton -> String -> Bool
accepts a
  = accepts' (startState a)
    where
      accepts' :: State -> String -> Bool
      accepts' s str
        = isTerminal s a && null str || any (try str) (transitionsFrom s a)
          where
            try :: String -> Transition -> Bool
            try str (_, t, Eps)
              = accepts' t str
            try "" _ 
              = False
            try (c' : str) (_, t, C c)
              = c' == c && accepts' t str


--------------------------------------------------------
-- Part III: 8/8

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

-- 8/8
make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make (Null) m n k
  = ([(m, n, Eps)], k) 
make (Term c) m n k
  = ([(m, n, C c)], k)
make (Seq r1 r2) m n k
  = (ts1 ++ ((k, k + 1, Eps) : ts2), k'')
    where
      (ts1, k') = make r1 m k (k + 2)
      (ts2, k'')   = make r2 (k + 1) n k'
make (Alt r1 r2) m n k
  = ((m, k, Eps) : (m, k + 2, Eps) : (k + 1, n, Eps) : (k + 3, n, Eps) : ts1 ++ ts2, k'')
    where
      (ts1, k') = make r1 k (k + 1) (k + 4)
      (ts2, k'') = make r2 (k + 2) (k + 3) k'
make (Rep r) m n k
  = ((m, k, Eps) : (m, n, Eps) : (k + 1, k, Eps) : (k + 1, n, Eps) : ts, k')
    where
      (ts, k') = make r k (k + 1) (k + 2)

--------------------------------------------------------
-- Part IV: 0.5/2

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

label :: Transition -> Label
label (_, _, lb)
  = lb

targetState :: Transition -> State
targetState (_, t, _)
  = t 

sourceState :: Transition -> State
sourceState (s, _, _)
  = s

targetStateSet :: (Label, [State]) -> [State]
targetStateSet (_, ss)
  = ss

getFrontier :: State -> Automaton -> [Transition]
getFrontier s a
  | isTerminal s a = (s, s, Eps) : concatMap ((flip getFrontier a) . targetState) teps ++ tneps
  | otherwise      = concatMap ((flip getFrontier a) . targetState) teps ++ tneps
    where
      teps = filter ((==Eps) . label) sts 
      tneps = filter ((/=Eps) . label) sts
      sts = transitionsFrom s a

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions ts
  = [(lb, [to | (_, to, lb') <- ts, lb == lb']) | lb <- labels ts] 

frontierMetastate :: [Transition] -> MetaState 
frontierMetastate
  = (sort . nub . map sourceState)

-- metaStateToState :: MetaState -> State
-- metaStateToState m
--   = 

-- 0.5/2: Decent attempt
makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA nda
  = undefined-- (1, map fst (zip ms [2..length ms + 1]), )
      
    where
      (m, ms, mts) = makeDA' [startState nda] [] []
      makeDA' :: [State] -> [MetaState] -> [MetaTransition]
             -> (MetaState, [MetaState], [MetaTransition])
      makeDA' ss ms ts
        | elem m ms = (m, ms, ts) 
        | otherwise = foldl (\(r, ms', ts') (lb, targets) -> makeDA' targets ms' ((m, r, lb) : ts')) ([], m : ms, ts) gts
          where
            frontier = concatMap (flip getFrontier nda) ss
            m       = frontierMetastate frontier
            gts      = groupTransitions frontier 
              -- = foldl ((\ss' -> makeDA' ss' (m : ms) ts) . targetStateSet) ([], m : ms, ts) gts


--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])
