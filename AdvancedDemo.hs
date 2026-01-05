-- AdvancedDemo.hs - Demonstrates advanced Haskell features
module AdvancedDemo where

-- Higher-order functions
mapFunction :: (a -> b) -> [a] -> [b]
mapFunction _ [] = []
mapFunction f (x:xs) = f x : mapFunction f xs

filterFunction :: (a -> Bool) -> [a] -> [a]
filterFunction _ [] = []
filterFunction p (x:xs)
    | p x = x : filterFunction p xs
    | otherwise = filterFunction p xs

foldFunction :: (a -> b -> b) -> b -> [a] -> b
foldFunction _ acc [] = acc
foldFunction f acc (x:xs) = f x (foldFunction f acc xs)

-- Composition and currying
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

addThenMultiply :: Int -> Int -> Int -> Int
addThenMultiply x y z = (x + y) * z

curriedAdd :: Int -> Int -> Int
curriedAdd x y = x + y

-- Partial application examples
add10 :: Int -> Int
add10 = curriedAdd 10

multiplyBy2 :: Int -> Int
multiplyBy2 = (* 2)

-- List comprehensions
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | c <- [1..n], 
                                   b <- [1..c], 
                                   a <- [1..b], 
                                   a^2 + b^2 == c^2]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], all (\y -> x `mod` y /= 0) [2..floor(sqrt(fromIntegral x))]]

-- Type class demonstrations
class Describable a where
    describe :: a -> String

instance Describable Int where
    describe n = "An integer: " ++ show n

instance Describable String where
    describe s = "A string: " ++ s

instance Describable [a] where
    describe xs = "A list with " ++ show (length xs) ++ " elements"

-- Maybe type demonstrations
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Recursive data structures
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x Empty = Node x Empty Empty
insertTree x (Node y left right)
    | x <= y = Node y (insertTree x left) right
    | otherwise = Node y left (insertTree x right)

searchTree :: (Ord a) => a -> Tree a -> Bool
searchTree _ Empty = False
searchTree x (Node y left right)
    | x == y = True
    | x < y = searchTree x left
    | otherwise = searchTree x right

-- Sample data
sampleTree :: Tree Int
sampleTree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) 
                   (Node 7 (Node 6 Empty Empty) (Node 9 Empty Empty))

-- Quick test functions
testHigherOrder :: [Int]
testHigherOrder = mapFunction (* 2) [1, 2, 3, 4, 5]

testFilter :: [Int]
testFilter = filterFunction (> 3) [1, 2, 3, 4, 5, 6]

testFold :: Int
testFold = foldFunction (+) 0 [1, 2, 3, 4, 5]

main :: IO ()
main = do
    putStrLn "=== Advanced Demo ==="
    putStrLn "Higher-order functions:"
    putStrLn $ "Map (*2) [1..5]: " ++ show testHigherOrder
    putStrLn $ "Filter (>3) [1..6]: " ++ show testFilter
    putStrLn $ "Fold (+) 0 [1..5]: " ++ show testFold
    putStrLn $ "Pythagorean triples up to 15: " ++ show (pythagoreanTriples 15)
    putStrLn $ "Primes up to 30: " ++ show (primes 30)
    putStrLn $ "Tree search for 4: " ++ show (searchTree 4 sampleTree)
