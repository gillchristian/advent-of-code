module Day03 where

type Coord = (Int, Int)

data Strategy = Strategy {right :: Int, down :: Int}

mkCoords :: Strategy -> Coord -> [Coord]
mkCoords strategy (h, w) = accer (h - 1) (0, 0)
  where
    step (y, x) = (y + down strategy, (x + right strategy) `mod` w)
    accer n coord
      | n <= 0 = []
      | otherwise = step coord : accer (n - down strategy) (step coord)

countTree :: [String] -> Int -> Coord -> Int
countTree grid trees (y, x)
  | grid !! y !! x == '#' = trees + 1
  | otherwise = trees

solve :: [String] -> Strategy -> Int
solve grid strategy =
  foldl (countTree grid) 0 $ mkCoords strategy (length grid, length $ head grid)

day03 :: IO ()
day03 = do
  input <- lines <$> readFile "input/day03.txt"
  putStrLn $ "Part 1: " <> show (solve input Strategy {down = 1, right = 3}) <> " trees"
  let strategiesP2 =
        [ Strategy {down = 1, right = 1},
          Strategy {down = 1, right = 3},
          Strategy {down = 1, right = 5},
          Strategy {down = 1, right = 7},
          Strategy {down = 2, right = 1}
        ]
  putStrLn $ ("Part 2: " <>) $ show $ product $ map (solve input) strategiesP2
