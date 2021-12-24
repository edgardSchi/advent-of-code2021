module Main where

main :: IO ()
main = do
    content <- readFile "input"
    let fileLines = lines content
    let values = map (map parseInput) fileLines
    putStrLn $ "Part one: " ++ show (part1 values)
    putStrLn $ "Part two: " ++ show (part2 values)


textToInt :: String -> Int
textToInt s = read s :: Int

parseInput :: Char -> Int
parseInput s = case s of
            '0' -> -1
            '1' -> 1
            _   -> undefined

undoParse :: Int -> Char
undoParse s = case s of
               -1 -> '0'
               1 -> '1'
               _ -> error "You done goofed"

part1 :: [[Int]] -> Int
part1 values = gammaRate values * epsilonRate values

gammaRate :: [[Int]] -> Int
gammaRate = rate (\x -> if x > 0 then 1 else 0)

epsilonRate :: [[Int]] -> Int
epsilonRate = rate (\x -> if x > 0 then 0 else 1)

rate :: (Int -> Int) -> [[Int]] -> Int
rate f = toDecimal . reverse . map f . foldr1 (zipWith (+))

-- The last element of the list is the most significant bit
toDecimal :: [Int] -> Int
toDecimal l = case l of
                [] -> 0
                x:xs -> case x of
                         0 -> 2 * toDecimal xs
                         1 -> 1 + (2 * toDecimal xs)
                         _ -> error "You done goofed"

part2 :: [[Int]] -> Int
part2 xs = oxygen xs * co2 xs 

rating :: (Int -> Int) -> [[Int]] -> Int
rating f vals = g vals 0 where g l n = case l of
                                        [] -> 0
                                        [x] -> toDecimal (reverse (map (\i -> if i > 0 then 1 else 0) x))
                                        xs  -> g (removeTargets (f $ countN n l) n l) (n+1)

oxygen :: [[Int]] -> Int
oxygen = rating id 

co2 :: [[Int]] -> Int
co2 = rating (\x -> if x == -1 then 1 else -1)

countN:: Int -> [[Int]] -> Int
countN n l = if (!! n) (foldr1 (zipWith (+)) l) >= 0 then 1 else -1

removeTargets :: Int -> Int -> [[Int]] -> [[Int]]
removeTargets n pos vals = case vals of
                        [] -> []
                        (x:xs) -> if x !! pos == n then x : removeTargets n pos xs else removeTargets n pos xs


