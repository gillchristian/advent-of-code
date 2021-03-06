module Day02 where

import Data.List (find)

-- INPUT

input :: [Int]
input =
  [1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 2, 19, 13, 23, 1, 23, 10, 27, 1, 13, 27, 31, 2, 31, 10, 35, 1, 35, 9, 39, 1, 39, 13, 43, 1, 13, 43, 47, 1, 47, 13, 51, 1, 13, 51, 55, 1, 5, 55, 59, 2, 10, 59, 63, 1, 9, 63, 67, 1, 6, 67, 71, 2, 71, 13, 75, 2, 75, 13, 79, 1, 79, 9, 83, 2, 83, 10, 87, 1, 9, 87, 91, 1, 6, 91, 95, 1, 95, 10, 99, 1, 99, 13, 103, 1, 13, 103, 107, 2, 13, 107, 111, 1, 111, 9, 115, 2, 115, 10, 119, 1, 119, 5, 123, 1, 123, 2, 127, 1, 127, 5, 0, 99, 2, 14, 0, 0]

pairs :: [(Int, Int)]
pairs = (,) <$> [0 .. 99] <*> [0 .. 99]

expectedOutput :: Int
expectedOutput = 19690720

-- UTILS

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replace (n -1) newVal xs

runOperation :: (Int -> Int -> Int) -> (Int, Int, Int) -> [Int] -> [Int] -> Int
runOperation operation (a, b, at) program rest = runMachine program' rest'
  where
    updated = (program !! a) `operation` (program !! b)
    program' = replace at updated program
    rest' = drop (length program' - length rest) program'

-- WARNING: not complete function (assumes a lot about the input program)
runMachine :: [Int] -> [Int] -> Int
runMachine (output : _) (99 : _) = output
runMachine program (1 : x : y : z : rest) =
  runOperation (+) (x, y, z) program rest
runMachine program (2 : x : y : z : rest) =
  runOperation (*) (x, y, z) program rest
runMachine _ _ = error "case not covered, sorry :)"

findOutput :: Int -> [Int] -> (Int, Int) -> Bool
findOutput expected machine (noun, verb) =
  expected == runMachine machine' machine'
  where
    machine' = replace 2 verb $ replace 1 noun machine

calculateResult :: (Int, Int) -> Int
calculateResult (noun, verb) = 100 * noun + verb

-- SOLUTIONS

part1 :: [Int] -> [Int] -> Int
part1 = runMachine

part2 :: Int -> [Int] -> [(Int, Int)] -> Maybe Int
part2 expected initialMachine =
  fmap calculateResult . find (findOutput expected initialMachine)

-- do the IO \o/

day02 :: IO ()
day02 = do
  putStrLn $ "Part 1: " ++ show (part1 input input)
  putStrLn $ "Part 2: " ++ show (part2 expectedOutput input pairs)
