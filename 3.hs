-- 9.6
sqList :: [Int] -> [Int]
sqList ns = map (\n -> n^2) ns

sumSq :: [Int] -> Int
sumSq ns = foldr (+) 0 ns

gtZero :: [Int] -> Bool
gtZero ns = foldr (\x acc -> x > 0) True ns

-- 9.7
minVal :: (Int -> Int) -> Int -> Int
minVal f n = minimum (map f [0..n]) 

allEq :: (Int -> Int) -> Int -> Bool
allEq f n = foldr (\x acc -> (x == comparison) && acc) True (map f [0..n])
    where comparison = f 0

allGtZero :: (Int -> Int) -> Int -> Bool
allGtZero f n = foldr (\x acc -> (x > 0) && acc) True (map f [0..n])

increasing :: (Int -> Int) -> Int -> Bool
increasing f n = 
    let vals = map f [0..n]
    in all (uncurry (<)) (zip vals (tail vals))

-- 9.9 
iter :: Int -> (Int -> Int) -> Int -> Int
iter n f x = (foldr (.) id (replicate n f)) x

-- 10.7

flipFunction :: (a -> b -> c) -> (b -> a -> c)
flipFunction f = \y x -> f x y

-- 10.13
flippedPipeline :: [Int] -> [Int]
-- flippedPipeline a = (filter (>0) . map (+1)) a
flippedPipeline = map (+1) . filter (> -1)


