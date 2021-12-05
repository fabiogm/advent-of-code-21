import Data.List
import System.IO

main = do
    contents <- readFile "data.txt"
    let measurements = map read . words $ contents
    print "a. Number increases"
    print $ countIncreases measurements
    print "b. Window increases"
    print $ countIncreases (windowSums measurements)


countIncreases :: [Int] -> Int
countIncreases [a] = 0
countIncreases (x:xs) = if x < head xs then 1 + countIncreases xs else countIncreases xs

windowSums :: [Int] -> [Int]
windowSums [a, b]     = []
windowSums (a:b:c:xs) = (a + b + c) : windowSums (b:c:xs)

-- alternative solutions
--countIncrease :: [Int] -> Int
--countIncrease [a] = 0
--countIncrease (x:xs) 
--  | x < head xs = 1 + countIncrease xs
--  | otherwise   = countIncrease xs

--countIncrease :: [Int] -> Int
--countIncrease [a]    = 0
--countIncrease (x:xs) = countIncrease xs + term 
--                       where term = if x < head xs then 1 else 0
                        
