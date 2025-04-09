-- 17.4
scalarProduct :: Floating a => [a] -> [a] -> a -> a
scalarProduct list1 list2 angle = sum( zipWith (\x y -> abs x * abs y * cos angle) list1 list2) 

-- 17.22
factorial :: Int -> [Int]
factorial 0 = [1]
factorial n = 1 : map (\ x -> foldr (*) 1 [2..x]) [1..n]

fibonacci :: Int -> [Int]
fibonacci 0 = [0]
fibonacci 1 = [1]
fibonacci n = take n fibs 
    where fibs = 0 : 1 :zipWith (+) fibs (tail fibs) 

   