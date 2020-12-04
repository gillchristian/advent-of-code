module Day04 where

import Control.Monad (void)
import Text.Parsec ((<|>))
import Text.Parsec as P
import Text.Parsec.Text (Parser, parseFromFile)

data FieldLabel
  = Byr -- Birth Year
  | Iyr -- Issue Year
  | Eyr -- Expiration Year
  | Hgt -- Height
  | Hcl -- Hair Color
  | Ecl -- Eye Color
  | Pid -- Passport ID
  | Cid -- Country ID
  deriving (Eq, Ord, Show)

data Field = Field FieldLabel String
  deriving (Ord, Show)

instance Eq Field where
  (Field la _) == (Field lb _) = la == lb

type Passport = [Field]

mkLabel :: String -> FieldLabel
mkLabel "byr" = Byr
mkLabel "iyr" = Iyr
mkLabel "eyr" = Eyr
mkLabel "hgt" = Hgt
mkLabel "hcl" = Hcl
mkLabel "ecl" = Ecl
mkLabel "pid" = Pid
mkLabel "cid" = Cid
mkLabel s = error $ "Invalid field label '" <> s <> "'"

field :: Parser Field
field = do
  key <- mkLabel <$> P.choice (fmap (P.try . P.string) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"])
  void $ P.char ':'
  value <- P.many $ P.noneOf "\n "
  pure $ Field key value

passport :: Parser Passport
passport = P.endBy1 field (void P.space <|> void P.newline <|> P.eof)

hasField :: FieldLabel -> Passport -> Bool
hasField label = elem (Field label "")

isValid :: Passport -> Bool
isValid pspt
  | length pspt == 8 = True
  | length pspt == 7 && notElem (Field Cid "") pspt = True
  | otherwise = False

day04 :: IO ()
day04 = do
  (Right passports) <- parseFromFile (P.sepBy passport P.space) "input/day04.txt"
  putStrLn $ "Part 01: " <> show (length $ filter isValid passports) <> " valid passports"
