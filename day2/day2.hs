module Main where

data Directions = Forward Int | Down Int | Up Int deriving Show

main :: IO ()
main = do
    content <- readFile "input"
    let fileLines = lines content
    let values = map parseInput fileLines
    putStrLn $ "Part one: " ++ show (part1 values)
    putStrLn $ "Part two: " ++ show (part2 values)

textToInt :: String -> Int
textToInt s = read s :: Int

parseInput :: String -> Directions
parseInput s = let w = words s in case head w of
                                    "forward" -> Forward $ textToInt (w !! 1)
                                    "up"      -> Up $ textToInt (w !! 1)
                                    "down"    -> Down $ textToInt (w !! 1)
                                    _ -> undefined

part1 :: [Directions] -> Int
part1 ds = f ds 0 0 -- forward unit, depth
            where 
                f (Forward v : ds) i j =  f ds (i+v) j
                f (Down v : ds) i j =  f ds i (j+v)
                f (Up v : ds) i j =  f ds i (j-v)
                f [] i j = i * j

part2 :: [Directions] -> Int
part2 ds = f ds 0 0 0 -- forward unit, depth, aim
            where 
                f (Forward v : ds) hor depth aim =  f ds (hor+v) (depth + aim*v) aim
                f (Down v : ds) hor depth aim =  f ds hor depth (aim+v)
                f (Up v : ds) hor depth aim =  f ds hor depth (aim-v)
                f [] hor depth aim = hor * depth