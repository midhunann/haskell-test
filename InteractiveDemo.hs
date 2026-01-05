-- InteractiveDemo.hs - Demonstrates interactive I/O functions
module InteractiveDemo where

-- Interactive functions for REPL demonstration
greetUser :: String -> IO ()
greetUser name = putStrLn $ "Hello, " ++ name ++ "! Welcome to Haskell!"

askAge :: IO ()
askAge = do
    putStrLn "What's your age?"
    ageStr <- getLine
    let age = read ageStr :: Int
    putStrLn $ "You are " ++ show age ++ " years old!"

simpleCalculator :: Int -> Int -> String -> String
simpleCalculator x y op
    | op == "+" = show x ++ " + " ++ show y ++ " = " ++ show (x + y)
    | op == "-" = show x ++ " - " ++ show y ++ " = " ++ show (x - y)
    | op == "*" = show x ++ " * " ++ show y ++ " = " ++ show (x * y)
    | op == "/" = show x ++ " / " ++ show y ++ " = " ++ show (x `div` y)
    | otherwise = "Unknown operation"

-- List generators
generateNumbers :: Int -> [Int]
generateNumbers n = [1..n]

generateSquares :: Int -> [Int]
generateSquares n = [x*x | x <- [1..n]]

generateEvens :: Int -> [Int]
generateEvens n = [x | x <- [1..n], even x]

-- String processing
wordCount :: String -> Int
wordCount = length . words

characterCount :: String -> Int
characterCount = length

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- Mathematical functions
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibs :: Int -> [Int]
fibs n = [fibonacci i | i <- [0..n]]

-- Quick test functions (no parameters)
testMessage :: String
testMessage = "Hello from Haskell Run!"

testNumber :: Int
testNumber = 2023

testList :: [Int]
testList = [1, 2, 3, 4, 5]

-- Functions that work great with REPL
quickMath :: Int -> Int -> Int
quickMath x y = x * y + x + y

processText :: String -> String
processText text = "Processed: " ++ reverse (map toUpper text)
  where
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

main :: IO ()
main = do
    putStrLn "=== Interactive Demo ==="
    putStrLn "This module is perfect for REPL interaction!"
    putStrLn "Try these in the REPL:"
    putStrLn "  greetUser \"Your Name\""
    putStrLn "  simpleCalculator 10 5 \"+\""
    putStrLn "  generateSquares 10"
    putStrLn "  wordCount \"Hello World from Haskell\""
    putStrLn "  fibs 10"
    putStrLn "  quickMath 7 3"
    putStrLn "  processText \"hello world\""
