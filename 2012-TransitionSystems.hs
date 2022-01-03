import Data.List
import Data.Maybe

import Debug.Trace

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

-- TOTAL: 30/30 (100%) (30/32 possible marks)
------------------------------------------------------
-- PART I: 8/8

-- 1/1
lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp k t
  = (snd . head) (filter ((==k) . fst) t)

-- 2/2
-- states :: LTS -> [State]
-- states lts
--   = nub (states' lts)
--     where
--       states' :: LTS -> [State]
--       states' []
--         = []
--       states' (((s, s'), _) : ss)
--         = s : s' : (states' ss)

-- * Alternative using concatMap
states :: LTS -> [State]
states lts
  = nub (concatMap (\((s, s'), _) -> [s, s']) lts)

-- 3/3
transitions :: State -> LTS -> [Transition]
transitions s
  = filter ((==s) . fst . fst)

-- 2/2
alphabet :: LTS -> Alphabet
alphabet
  = nub . map snd
    
------------------------------------------------------
-- PART II

-- 5/5
-- actions :: Process -> [Id]
-- actions STOP
--   = [] 
-- actions (Ref _)
--   = []
-- actions (Prefix a p)
--   = a : (actions p)
-- actions (Choice ps)
--   = concatMap actions ps

-- * Could have been simplified by grouping the STOP and Ref at the end
actions :: Process -> [Id]
actions (Prefix a p)
  = a : actions p
actions (Choice ps)
  = concatMap actions ps
actions _ 
  = [] 


-- 6/6
accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts as defs@((_, p) : ds)
  = accepts' as p 
    where
      accepts' :: [Id] -> Process -> Bool
      accepts' [] _
        = True 
      accepts' as (Ref id)
        = accepts' as (lookUp id defs)
      accepts' (a : as) (Prefix id p)
        | a == id = accepts' as p
        | otherwise = False
      accepts' as (Choice ps)
        = or (map (accepts' as) ps) -- Is this valid? not 100% sure
      accepts' _ STOP
        = False


------------------------------------------------------
-- PART III

-- 5/5
--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions :: Transition -> Transition 
                       -> Alphabet -> Alphabet 
                       -> StateMap 
                       -> [Transition]
composeTransitions ((s, t), a) ((s', t'), a') a1 a2 m
  | a == a'                 = [((source, lookUp (t, t') m), a)] 
  | elem a a2 && elem a' a1 = []
  | elem a' a1              = [transitiona]
  | elem a a2               = [transitiona']
  | otherwise               = [transitiona, transitiona']
  where
    source = lookUp (s, s') m
    transitiona = ((source, lookUp (t, s') m), a)
    transitiona' = ((source, lookUp (s, t') m), a')

-- 2/4: Needs nub to remove duplicate transitions, a more efficient implementation would not visit states twice
-- pruneTransitions :: [Transition] -> LTS
-- pruneTransitions ts
--   = visit 0 []
--     where
--       visit :: State -> [State] -> [Transition]
--       visit s visited
--         | elem s visited = []
--         | otherwise      = ts' ++ (concatMap ((flip visit visited') . snd . fst) ts')
--         -- | otherwise = ts' ++ visit' (map (snd . fst) ts') visited'
--           where
--             ts' = transitions s ts
--             visited' = s : [from | ((from, _), _) <- ts'] ++ visited
--             -- visit' :: [State] -> [State] -> [Transition]
--             -- visit' [] _
--             --   = []
--             -- visit' (s : ss) visited
--             --   | elem s visited = []
--             --   | otherwise = ts' ++ (visit' (ss ++ (map (snd . fst) ts')) visited')
--             --     where
--             --       ts' = transitions s ts
--             --       visited' = s : [from | ((from, _), _) <- ts'] ++ visited

-- * nub has to be added since we are not updating the visited list for each mapping in concatMap thus we are visiting some things twice
pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = nub (visit 0 [])
    where
      visit :: State -> [State] -> [Transition]
      visit s visited
        | elem s visited = []
        | otherwise      = ts' ++ (concatMap ((flip visit (s : visited)) . snd . fst) ts')
          where
            ts' = transitions s ts
            

------------------------------------------------------
-- PART IV

-- 4/4
compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = pruneTransitions (compose' ps)
    where
      a1 = "$'" : alphabet lts1
      a2 = "$" : alphabet lts2
      ss = states lts1
      ss' = states lts2
      ps = cartesianProduct ss ss' 
      m   = zip ps [0..]
      compose' :: [(State, State)] -> LTS
      compose' []
        = []
      compose' ((s, s') : ps)
        = (concatMap (\(t1, t2) -> composeTransitions t1 t2 a1 a2 m) tps) ++ compose' ps
          where
            ts1 = ((s, -1), "$") : transitions s lts1
            ts2 = ((s', -1), "$'") : transitions s' lts2
            tps = cartesianProduct ts1 ts2
            

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys
  = [(x, y) | x <- xs, y <- ys]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS
  = undefined
-- buildLTS defs@((initialID, p) : ds)
--   = lts 
--   where
--     (lts, _) = buildLTS' p 0 1
-- 
-- buildLTS' :: Process -> State -> State -> (LTS, State)
-- buildLTS' (Ref i) s s'
--   = lookUp i defs

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
