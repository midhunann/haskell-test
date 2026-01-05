-- DataStructures.hs - Demonstrates data types and pattern matching
module DataStructures where

-- Custom data types
data Person = Person String Int deriving (Show, Eq)

data Color = Red | Green | Blue | RGB Int Int Int deriving (Show, Eq)

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float deriving (Show, Eq)

-- Functions working with custom types
getName :: Person -> String
getName (Person name _) = name

getAge :: Person -> Int
getAge (Person _ age) = age

createPerson :: String -> Int -> Person
createPerson name age = Person name age

-- Color functions
colorToString :: Color -> String
colorToString Red = "Red"
colorToString Green = "Green"
colorToString Blue = "Blue"
colorToString (RGB r g b) = "RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

-- Shape calculations
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Triangle a b c) = a + b + c

-- List operations with custom types
findPersonByName :: String -> [Person] -> Maybe Person
findPersonByName _ [] = Nothing
findPersonByName name (p@(Person n _):ps)
    | name == n = Just p
    | otherwise = findPersonByName name ps

averageAge :: [Person] -> Float
averageAge [] = 0
averageAge people = fromIntegral (sum ages) / fromIntegral (length ages)
    where ages = map getAge people

-- Sample data
samplePeople :: [Person]
samplePeople = [Person "Alice" 25, Person "Bob" 30, Person "Charlie" 22]

sampleShapes :: [Shape]
sampleShapes = [Circle 5.0, Rectangle 4.0 6.0, Triangle 3.0 4.0 5.0]

-- Demo functions
testPerson :: Person
testPerson = Person "Demo User" 25

testColor :: Color
testColor = RGB 255 128 0

testShape :: Shape
testShape = Circle 3.14

main :: IO ()
main = do
    putStrLn "=== Data Structures Demo ==="
    putStrLn $ "Sample person: " ++ show testPerson
    putStrLn $ "Person name: " ++ getName testPerson
    putStrLn $ "Person age: " ++ show (getAge testPerson)
    putStrLn $ "Color: " ++ colorToString testColor
    putStrLn $ "Shape: " ++ show testShape
    putStrLn $ "Circle area: " ++ show (area testShape)
    putStrLn $ "Average age: " ++ show (averageAge samplePeople)
