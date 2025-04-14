import Data.List (sort)

-- 18.2
readTwoInt :: IO ()
readTwoInt = do
    putStrLn "Enter two integers"
    input1 <- getLine
    input2 <- getLine
    let number1 = read input1 :: Int
        number2 = read input2 :: Int
        result = number1 + number2
    putStrLn (show result)

-- 18.3
calcSum :: IO ()
calcSum = do
    putStrLn "Enter numbers to sum (press Enter on an empty line to finish):"
    total <- sumNumbers 0
    putStrLn $ "The total sum is: " ++ show total

sumNumbers :: Int -> IO Int
sumNumbers acc = do
    line <- getLine
    if null line
       then return acc
       else do
            let num = read line :: Int
            sumNumbers (acc + num)

-- 18.5   
sortInputList :: IO ()
sortInputList = do
    putStrLn "Enter integers to be sorted (press Enter on an empty line to finish)"
    intList <- inputIntegers []
    let sortedList = sort intList -- used the imported sort for ease
    putStrLn (show sortedList)

inputIntegers :: [Int] -> IO [Int]
inputIntegers list = do
    line <- getLine
    if null line
        then return (reverse list)
        else do
            let num = read line :: Int
            inputIntegers (num : list)

-- 18.6
mapF :: (a -> b) -> IO a -> IO b
mapF f action = do
    f <$> action
