import Data.List
import System.IO

main = do
    contents <- readFile "data.txt"
    let commands = fmap words $ lines contents
    let parsedCommands = map parseCommand commands
    let finalStateA = foldl executeCommand2 (0, 0) parsedCommands
    print "a."
    print $ fst finalStateA * snd finalStateA
    let finalStateB = foldl executeCommand3 (0, 0, 0) parsedCommands
    let (position, depth, aim) = finalStateB
    print "b."
    print $ position * depth

parseCommand :: [String] -> (String, Int)
parseCommand xs = (head xs, read (last xs) :: Int)

-- Apply command to state.
executeCommand2 :: (Int, Int) -> (String, Int) -> (Int, Int) 
executeCommand2 (position, depth) ("forward", arg) = 
    (position + arg, depth)
executeCommand2 (position, depth) ("up", arg) =
    (position, depth - arg)
executeCommand2 (position, depth) ("down", arg) =
    (position, depth + arg)

executeCommand3 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
executeCommand3 (position, depth, aim) ("forward", arg) =
    (position + arg, depth + aim * arg, aim)
executeCommand3 (position, depth, aim) ("up", arg) =
    (position, depth, aim - arg)
executeCommand3 (position, depth, aim) ("down", arg) =
    (position, depth, aim + arg)

