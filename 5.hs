-- 14.1
data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter
        deriving (Eq, Show)

weather :: Season -> Temp
weather season
    | season == Summer = Hot
    | otherwise = Cold

-- alternatively 

-- weather :: Season -> Temp
-- weather season =
--     if season == Summer
--         then Hot
--         else Cold

-- 14.4
data Shape = Circle Float | Rectangle Float Float 
        deriving (Eq, Ord, Show, Read)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * 3.14 * r
perimeter (Rectangle w h) = 2*w + 2*h 

-- 14.17
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr | IntDiv Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add y x) = eval y + eval x 
eval (Sub y x) = eval y - eval x
eval (Mult y x) = eval y * eval x
eval (IntDiv y x) =
    if eval x == 0
        then error "Division by zero"
        else eval y `div` eval x

show :: Expr -> IO()
show (Lit n) = print n
show (Add y x) = print (eval y , " Add " , eval x , " = " , eval y + eval x)
-- etc


size :: Expr -> Int
size (Lit n) = 1
size (Add y x) = 2
-- etc

-- 14.33
data GTree = Empty | Gtree Int [GTree] deriving (Show)

someTree :: GTree
someTree = Gtree 0 []

-- Invoke this with 'constructTree [1, 2, 3] someTree'
constructTree :: [Int] -> GTree
constructTree [] = Empty
constructTree  [x]  = Gtree x []
constructTree (x:xs) = Gtree x [constructTree xs]

countLeaves :: GTree -> Int
countLeaves Empty = 0
countLeaves (Gtree _ []) = 1
countLeaves (Gtree _ children) = sum (map countLeaves children)

findDepth :: GTree -> Int
findDepth Empty = 0
findDepth (Gtree _ []) = 0
findDepth (Gtree _ children) = 1 + sum (map findDepth children)

sumGtree :: GTree -> Int
sumGtree Empty = 0
sumGtree (Gtree value []) = value
sumGtree (Gtree value children) = value + sum (map sumGtree children)

-- Helper function to flatten our GTree node list
flattenGTree :: GTree -> [Int]
flattenGTree (Gtree n children) = n : concatMap flattenGTree children

findElement :: Int -> GTree -> Bool
findElement element Empty = False
findElement element (Gtree n children) 
    | length [y | (y,z) <- zip [1..] elementList, z == element] > 0 = True
    | otherwise = False
    where elementList = flattenGTree (Gtree n children)

mapFunctionToTree :: (Int -> Int) -> GTree -> GTree
mapFunctionToTree f Empty = Empty
mapFunctionToTree f (Gtree n children) = constructTree (map f elementList)
    where elementList = flattenGTree (Gtree n children)

