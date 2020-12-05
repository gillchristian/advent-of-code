module Day03 where

type Coord = (Int, Int)

data Strategy = Strategy {right :: Int, down :: Int}

mkCoords :: Strategy -> Int -> [Coord]
mkCoords strategy h = go (h - 1) (0, 0)
  where
    step (y, x) = (y + down strategy, x + right strategy)
    go n coord
      | n <= 0 = []
      | otherwise = step coord : go (n - down strategy) (step coord)

countTree :: [String] -> Int -> Coord -> Int
countTree grid count (y, x)
  | grid !! y !! x == '#' = count + 1
  | otherwise = count

solve :: [String] -> Strategy -> Int
solve grid strategy =
  foldl (countTree grid) 0 $ mkCoords strategy $ length grid

day03 :: IO ()
day03 = do
  input <- fmap cycle . lines <$> readFile "input/day03.txt"
  let strategy1 = Strategy {down = 1, right = 3}
  putStrLn $ "Part 1: " <> show (solve input strategy1) <> " trees"
  let strategiesP2 =
        [ Strategy {down = 1, right = 1},
          strategy1,
          Strategy {down = 1, right = 5},
          Strategy {down = 1, right = 7},
          Strategy {down = 2, right = 1}
        ]
  putStrLn $ ("Part 2: " <>) $ show $ product $ map (solve input) strategiesP2
