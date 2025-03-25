-- 3.7
-- Will return false when we input 3 4 3 since m == p

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p =
    not (m == n || m == p)

-- 3.8
-- The two functions fourEqual and fourEqual_2 returns the same answer

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q =
    m == n && m == p && m == q

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p =
    m == n && m == p

fourEqual_2 :: Int -> Int -> Int -> Int -> Bool
fourEqual_2 m n p q = 
    threeEqual m n p && m == q 

-- 3.15
-- Return 0, 1, or 2 based on the discriminant of the quadratic function
-- For example, 1 2 1 returns 1

numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c  
    | discriminant < 0 = 0
    | discriminant == 0 = 1
    | otherwise = 2

    where discriminant = b^2 - 4*a*c

-- 3.16
-- Returns 3 if we input 0 0 0. If a b c != 0, then we call NumberNDroots 

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c 
    | a == 0 && b == 0 && c == 0 = 3
    | otherwise = numberNDroots a b c

-- 3.17 
-- Returns the smaller of the two roots and the larger of the two roots depending on which of the functions you use


smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
    | nr == 0 = 0
    | nr == 3 = 0
    | nr == 1 = oneRoot
    | nr == 2 = twoRootsSmaller
    where 
        nr = numberRoots a b c
        disc = b^2 - 4*a*c
        oneRoot = -b / (2*a)
        twoRootsSmaller = (-b - sqrt disc) / (2*a)

largerRoot a b c 
    | nr == 0 = 0
    | nr == 3 = 0
    | nr == 1 = oneRoot
    | nr == 2 = twoRootsLarger
    where 
        nr = numberRoots a b c
        disc = b^2 - 4*a*c
        oneRoot = -b / (2*a)
        twoRootsLarger = (-b + sqrt disc) / (2*a)

-- 4.7
-- If we input 2 2, we get 4. Likewise if we input 2 4 we get 8.

recursiveMultiplication :: Int -> Int -> Int
recursiveMultiplication a b 
    | b == 0 = 0
    | b > 0 = a + recursiveMultiplication a (b-1)

-- 4.8


recursiveSqrt :: Int -> Int
recursiveSqrt 0 = 0
recursiveSqrt n =
    let r = recursiveSqrt (n - 1)
    in if (r + 1) * (r + 1) <= (n + 1)
        then r + 1
        else r 




    










