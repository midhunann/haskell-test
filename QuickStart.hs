-- QuickStart.hs - Perfect for quick demo showing all features
module QuickStart where

-- Simple functions for CodeLens demo
hello :: String
hello = "Hello from Haskell Run!"

addTwo :: Int -> Int -> Int
addTwo x y = x + y

square :: Int -> Int
square x = x * x

-- Function with no parameters (great for CodeLens)
meaningOfLife :: Int
meaningOfLife = 42

-- Function with one parameter
double :: Int -> Int
double x = x * 2

-- Function with multiple parameters
calculator :: Int -> Int -> String -> Int
calculator x y op
    | op == "+" = x + y
    | op == "-" = x - y
    | op == "*" = x * y
    | op == "/" = x `div` y
    | otherwise = 0

-- List functions
sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- String functions
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Boolean functions
isPositive :: Int -> Bool
isPositive x = x > 0

-- Main function for file execution
main :: IO ()
main = do
    putStrLn "🚀 Welcome to Haskell Run Demo!"
    putStrLn "================================"
    putStrLn ""
    putStrLn "✨ Features to demonstrate:"
    putStrLn "1. Click the ▶️ button to run this file"
    putStrLn "2. Use CodeLens ▶️ Run buttons above functions"
    putStrLn "3. Check the Functions panel in the sidebar"
    putStrLn "4. Try keyboard shortcuts (F5, Shift+F5)"
    putStrLn "5. Select function names and run them"
    putStrLn ""
    putStrLn "🎯 Try these functions:"
    putStrLn "• hello (no parameters)"
    putStrLn "• addTwo 5 3"
    putStrLn "• square 7"
    putStrLn "• greet \"World\""
    putStrLn "• calculator 10 3 \"+\""
    putStrLn "• sumNumbers [1,2,3,4,5]"
    putStrLn ""
    putStrLn "Demo completed! 🎉"
