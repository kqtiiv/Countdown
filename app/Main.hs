module Main where

import Program.Operations
import Program.Program (solutions'')
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- random number between 1 and 10
getSource :: IO Int 
getSource = randomRIO(start, end)
    where 
        start = 1
        end = 10
-- random number between 100 and 999
getTarget :: IO Int 
getTarget = randomRIO(start, end) 
    where 
        start = 100
        end = 999

-- generate the list of numbers given 2 random numbers
genList :: Int -> Int -> [Int]
genList n n' = (min n n'):(max n n'):defaultNums
    where defaultNums = [50, 75, 100]

-- parse the user's answer
-- doParseAction :: String -> (String -> Maybe a) -> IO a 
-- doParseAction errMsg f = do 
--     input <- getLine 
--     case f input of 
--         Nothing -> do 
--             putStrLn errMsg 
--             doParseAction errMsg f 
--         Just val -> return val 


-- better parse using monads
untilJustM :: Monad m => m (Maybe a) -> m a 
untilJustM mmse = mmse >>= maybe (untilJustM mmse) return 

doParseAction :: String -> (String -> Maybe a) -> IO a 
doParseAction msg validate = untilJustM (validate <$> (putStr msg *> getLine))

-- check if use is correct 
checkAns :: Int -> Expr -> [Bool]
checkAns target ans = do 
    res <- eval ans
    return $ res==target 

main :: IO ()
main = do 
    putStrLn "Welcome to Countdown!"
    putStrLn "Press Enter to start!"
    line <- getLine 

    num1 <- getSource
    num2 <- getSource
    let nums = genList num1 num2
    target <- getTarget

    putStrLn "You have 60 seconds."
    putStrLn $ "Here are your digits: " ++ show nums 
    putStrLn $ "Target: " ++ show target

    putStrLn "Answer: "
    userAns <- doParseAction "Invalid" readMaybe

    if checkAns target (read userAns) 
    then putStrLn "Correct!"
    else putStrLn "Incorrect!"
    
    return ()




