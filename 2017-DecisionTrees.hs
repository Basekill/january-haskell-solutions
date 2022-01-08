import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

-- TOTAL: 25.5/30 (85%)
--------------------------------------------------------------------
-- PART I: 12/14
--------------------------------------------------------------------

-- 2/2
-- allSame :: Eq a => [a] -> Bool
-- allSame xs
--   = (null . filter (/= head xs)) xs

-- * Alternative using all
allSame :: Eq a => [a] -> Bool
allSame []
  = True
allSame (x : xs)
  = all (==x) xs

-- 2/2
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove k xys
  = filter ((/=k) . fst) xys -- we can use extensionality

-- 2/2
lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt n h r
  = r !! (fromJust . findIndex ((==n) . fst)) h
--  = r !! lookUp n (zip (map fst h) [0..])

-- * elemIndex can be used instead of findIndex (==n)
-- lookUpAtt :: AttName -> Header -> Row -> AttValue
-- --Pre: The attribute name is present in the given header.
-- lookUpAtt n h r
--   = r !! (fromJust (elemIndex n (map fst h)))

-- removeAtt :: AttName -> Header -> Row -> Row
-- removeAtt n h r
--   | isNothing i = r 
--   | otherwise = (map fst . filter ((/= fromJust i) . snd)) (zip r [0..])
--     where
--       i = lookup n (zip (map fst h) [0..]) 

-- 2/2
removeIndex :: Int -> [a] -> [a]
removeIndex i xs
  = as ++ tail bs 
    where
      (as, bs) = splitAt i xs

removeAtt :: AttName -> Header -> Row -> Row
removeAtt n h r
  | isNothing i = r 
  | otherwise = removeIndex (fromJust i) r
    where
      i = findIndex ((==n) . fst) h 

-- 2/3: No need to use lookup nor remove
-- addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
-- addToMapping (x, v) m
--   | isNothing vs = (x, [v]) : m
--   | otherwise     = (x, v : fromJust vs) : (remove x m)
--     where
--       vs = lookup x m

-- * Use recursion to find the element rather than lookup
addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, v) []
  = [(x, [v])]
addToMapping xv@(x, v) (xvs@(x', vs) : xvss)
  | x == x' = (x, v : vs) : xvss
  | otherwise     = xvs : addToMapping xv xvss

-- 2/3: finding the index is not required since if it doesn't exist, then a the length of a list from a filter is 0
-- buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
-- --Pre: Each row of the data set contains an instance of the attribute
-- buildFrequencyTable (n, vs) (h, rs)
--   | isNothing i = zip vs (repeat 0)
--   | otherwise   = map (\v -> (v, length (filter (==v) rs'))) vs
--     where
--       i = findIndex ((==n) . fst) h 
--       rs' = map (!! fromJust i) rs

-- * We can use map (lookUpAtt n h) rs instead of map (!! fromJust i) rs
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
buildFrequencyTable (n, vs) (h, rs)
  = map (\v -> (v, length (filter (==v) (map (lookUpAtt n h) rs)))) vs
  

--------------------------------------------------------------------
-- PART II: 5/5
--------------------------------------------------------------------

-- 2/2
nodes :: DecisionTree -> Int
nodes Null
  = 0 
nodes (Leaf _)
  = 1
nodes (Node _ vts)
  = 1 + sum (map (nodes . snd) vts)

-- 3/3
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = "" 
evalTree (Leaf v) _ _
  = v
evalTree (Node n vts) h r
  = evalTree (lookUp (lookUpAtt n h r) vts) h r


--------------------------------------------------------------------
-- PART III: 8/9
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

-- 5/5
partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (n, vs)
  = [(v, (remove n h, map (removeAtt n h) (filter (elem v) rs))) | v <- vs]


-- 3/4: You must use sel rather than nextAtt, nextAtt is a specific instance of sel
-- buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
-- buildTree (_, []) _ _ 
--   = Null 
-- buildTree d@(h, rs) ca@(cn, _) sel
--   | allSame crs = Leaf (head crs)
--   | otherwise   = Node n (map (\(v, d') -> (v, buildTree d' ca sel)) p)  
--     where
--       a@(n, vs) = nextAtt d ca -- * Use sel
--       p = partitionData d a
--       crs = map (lookUpAtt cn h) rs

-- * Uses sel rather than nextAtt
buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_, []) _ _ 
  = Null 
buildTree d@(h, rs) ca@(cn, _) sel
  | allSame crs = Leaf (head crs)
  | otherwise   = Node n (map (\(v, d') -> (v, buildTree d' ca sel)) p)  
    where
      a@(n, vs) = sel d ca
      p         = partitionData d a
      crs       = map (lookUpAtt cn h) rs

--------------------------------------------------------------------
-- PART IV: 0.5/2
--------------------------------------------------------------------

-- 0.5/2: Finished prob and entropy
prob :: DataSet -> Attribute -> AttValue -> Double
prob d@(_, rs) a v
  = fromIntegral (lookUp v fs) / fromIntegral (length rs)
    where
      fs = buildFrequencyTable a d

-- entropy :: DataSet -> Attribute -> Double
-- entropy (_, []) _
--   = 0.0
-- entropy d a@(_, vs)
--   = sum (map (\v -> - (xlogx . prob d a) v) vs) 

-- * lambda function can be simplified
entropy :: DataSet -> Attribute -> Double
entropy (_, []) _
  = 0.0
entropy d a@(_, vs)
  = - (sum . map (xlogx . prob d a)) vs

-- * Extra time
-- * Only works if partitionData partitions the data in the same order as vs
-- gain :: DataSet -> Attribute -> Attribute -> Double
-- gain d p@(_, vs) c
--   = entropy d c - sum [(prob d p v) * (entropy dp c) | (v, (_, dp)) <- zip vs (partitionData d p)] 

-- * Github solution using a lookUp to make sure the value is correct, however worse time complexity
gain :: DataSet -> Attribute -> Attribute -> Double
gain d p@(_, vs) c
  = entropy d c - sum ((map reduce) vs)
    where
      reduce :: AttValue -> Double
      reduce v = (prob d p v) * (entropy (lookUp v (partitionData d p)) c)


-- * Extra time
-- bestGainAtt :: AttSelector
-- bestGainAtt d@(h, _) ca@(cn, _)
--   = fst (foldl1 keepMax (map (\p -> (p, gain d p ca)) (remove cn h)))
--   where
--     keepMax :: Ord b => (a, b) -> (a, b) -> (a, b)
--     keepMax a@(_, y1) b@(_, y2)
--       | y1 >= y2  = a 
--       | otherwise = b 

-- * Alternative using elemIndex and maximum
bestGainAtt :: AttSelector
bestGainAtt d@(h, _) ca@(cn, _)
  = h' !! (fromJust . elemIndex (maximum gains)) gains
    where
      h' = remove cn h
      gains = map (\p -> gain d p ca) h'

-- bestGainAtt :: AttSelector
-- bestGainAtt ds@(h, _) att@(an, _)
--   = h' !! (fromJust . elemIndex (maximum gains)) gains
--   where
--     h'    = remove an h
--     gains = map (\attp -> gain ds attp att) h'

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]