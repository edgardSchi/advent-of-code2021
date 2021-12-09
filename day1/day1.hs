module Main where
import GHC.Base
import Data.List

main :: IO ()
main = do
    content <- readFile "input"
    let fileLines = lines content
    let values = map textToInt fileLines
    putStrLn $ "Part one: " ++ show (part1 values)
    putStrLn $ "Part two: " ++ show (part2 values)

part1 :: [Int] -> Int
part1 values = countIncreases values 0

part2 :: [Int] -> Int
part2 values = countIncreases (threeSum values) 0

-- part 1
countIncreases :: [Int] -> Int -> Int
countIncreases [] i = i
countIncreases [x] i = i
countIncreases (x : y : as) i = if y > x then countIncreases (y : as) (i+1) else countIncreases (y : as) i

threeSum :: [Int] -> [Int]
threeSum (x:y:z:as) = (x+y+z) : threeSum (y:z:as)
threeSum _ = []


textToInt :: String -> Int
textToInt s = read s :: Int