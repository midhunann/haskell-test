-- FunctionDemo.hs - Demonstrates function execution with parameters
module FunctionDemo where

-- Functions with different parameter types
multiply :: Int -> Int -> Int
multiply x y = x * y

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- String operations
reverseString :: String -> String
reverseString = reverse

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs
  where
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-- List functions
filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
    | even x = x : filterEven xs
    | otherwise = filterEven xs

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = (x * 2) : doubleList xs

-- Boolean functions
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..floor(sqrt(fromIntegral n))]

-- Functions that demonstrate CodeLens
quickTest :: Int
quickTest = 42

simpleMessage :: String
simpleMessage = "This is a simple message!"

main :: IO ()
main = do
    putStrLn "=== Function Demo ==="
    putStrLn "Use CodeLens or TreeView to run individual functions!"
    putStrLn "Try running: multiply 6 7"
    putStrLn "Try running: power 2 8"
    putStrLn "Try running: reverseString \"hello\""
    putStrLn "Try running: capitalize \"haskell\""
    putStrLn "Try running: filterEven [1,2,3,4,5,6,7,8,9,10]"
    putStrLn "Try running: doubleList [1,2,3,4,5]"
    putStrLn "Try running: isPrime 17"
