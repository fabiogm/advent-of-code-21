import Data.Char
import Data.List
import System.IO
import Data.Char(digitToInt)

main = do
    contents <- readFile "data.txt"
    let threshold = 500
    let diagnostics = lines contents
    let bitCounts = foldl updateBitCounts [0,0,0,0,0,0,0,0,0,0,0,0] diagnostics
    let gamma = gammaRate bitCounts
    print $ gamma
    let epsilon = invert gamma
    print $ epsilon
    let gammaNum = convert $ reverse gamma
    let epsilonNum = convert $ reverse epsilon
    print "a."
    print $ gammaNum * epsilonNum
    print $ "b."
    let pattern = mostCommon bitCounts
    print $ (filterPattern pattern 0 diagnostics)
    let oxygen = head (filterPattern pattern 0 diagnostics)
    let temp = (filterPattern (invertStr pattern) 0 diagnostics)
    print $ temp
    let co2 = head temp
    let oxygenVal = convert (reverse (toInt oxygen))
    let co2Val = convert (reverse ( toInt co2))
    print $ oxygenVal * co2Val

-- Right answer for part a: 1307354

updateBitCounts :: [Int] -> [Char] -> [Int]
updateBitCounts [x] [s]       = if s == '1' then [x + 1] else [x] 
updateBitCounts (x:xs) (s:ss) = x + inc : updateBitCounts xs ss
                                    where
                                        inc = if s == '1' then 1 else 0  

aboveThreshold :: Int -> Bool
aboveThreshold x = x > 500

gammaRate :: [Int] -> [Int]
gammaRate [x]    = if aboveThreshold x then [1] else [0]
gammaRate (x:xs) = if aboveThreshold x then 1 : gammaRate xs else 0 : gammaRate xs

invert :: [Int] -> [Int]
invert [x]    = if x == 1 then [0] else [1]
invert (x:xs) = if x == 1 then 0 : invert xs else 1 : invert xs

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

mostCommon :: [Int] -> String
mostCommon [x]    = if x >= 500 then "1" else "0"
mostCommon (x:xs) = if x >= 500 then "1" ++ mostCommon xs else "0" ++ mostCommon xs

filterPos :: Char -> Int -> [String] -> [String]
filterPos c i [s]    = if s !! i == c then [s] else []
filterPos c i (s:ss) = if s !! i == c then s : filterPos c i ss else filterPos c i ss

filterPattern :: String -> Int -> [String] -> [String]
filterPattern p i [s] = [s]
filterPattern p i ss = if length lis == 1 then lis else filterPattern p (i+1) lis
                              where lis = (filterPos (p!!i) i ss)

toInt :: String -> [Int]
toInt [c]    = if c == '1' then [1] else [0]
toInt (s:ss) = if s == '1' then 1 : toInt ss else 0 : toInt ss

invertStr :: String -> String
invertStr [x]    = if x == '1' then "0" else "1"
invertStr (x:xs) = if x == '1' then "0" ++ invertStr xs else "1" ++ invertStr xs

