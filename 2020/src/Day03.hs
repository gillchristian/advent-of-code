module Day03 where

type Coord = (Int, Int)

mkCoordinates :: Coord -> [Coord]
mkCoordinates (h, w) = accer (h - 1) (0, 0)
  where
    step (y, x) = (y + 1, (x + 3) `mod` w)
    accer 1 coord = [step coord]
    accer n coord = step coord : accer (n - 1) (step coord)

counter :: [String] -> (Int, Int) -> Coord -> (Int, Int)
counter grid (trees, empties) (y, x) =
  case (grid !! y) !! x of
    '.' -> (trees, empties + 1)
    '#' -> (trees + 1, empties)
    c -> error $ "Got unexpected char '" <> [c] <> "'"

day03 :: IO ()
day03 = do
  input <- lines <$> readFile "input/day03.txt"
  let h = length input
      w = length $ head input
      (trees, _) = foldl (counter input) (0, 0) $ mkCoordinates (h, w)
  putStrLn $ "Part 1: " <> show trees <> " trees"
