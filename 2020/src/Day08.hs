{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day08 where

import Data.Foldable (find)
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Instruction
  = NoOp Int
  | Acc Int
  | Jump Int
  deriving (Show, Eq)

type InstructionSet = IntMap (Instruction, Bool)

type Program = (Int, Int, InstructionSet)

mkInstruction :: [Text] -> Either String Instruction
mkInstruction ["nop", n] = Right $ NoOp $ read $ T.unpack n
mkInstruction ["acc", n] = Right $ Acc $ read $ T.unpack n
mkInstruction ["jmp", n] = Right $ Jump $ read $ T.unpack n
mkInstruction ln =
  Left $ "Got the wrong stuff '" <> T.unpack (T.unwords ln) <> "'"

parseProgram :: Text -> Either String [Instruction]
parseProgram =
  traverse mkInstruction . fmap T.words . T.lines . T.replace "+" ""

data Result
  = Loop Program
  | Terminated Program
  | OutOfBound Program
  deriving (Show, Eq)

terminated :: Result -> Bool
terminated (Terminated _) = True
terminated _ = False

runProgram :: Program -> Result
runProgram program@(_, i, instructionSet)
  | i == size = Terminated program
  | otherwise = maybe (OutOfBound program) runInstruction $ instructionSet !? i
  where
    size = IntMap.size instructionSet
    runInstruction (_, True) = Loop program
    runInstruction (instruction, _) = runProgram $ step instruction program

    visit :: IntMap.Key -> InstructionSet -> InstructionSet
    visit = IntMap.adjust (fmap $ const True)

    step :: Instruction -> Program -> Program
    step (NoOp _) (c, i, is) = (c, i + 1, visit i is)
    step (Jump to) (c, i, is) = (c, i + to, visit i is)
    step (Acc acc) (c, i, is) = (c + acc, i + 1, visit i is)

mkInstructionSet :: [Instruction] -> InstructionSet
mkInstructionSet = IntMap.fromList . zip [0 ..] . fmap (,False)

invertInstruction :: Instruction -> Instruction
invertInstruction (NoOp n) = Jump n
invertInstruction (Jump n) = NoOp n
invertInstruction i = i

mkVariations :: IntMap Instruction -> [InstructionSet]
mkVariations instructions =
  IntMap.map (,False) . variation <$> [0 .. IntMap.size instructions]
  where
    variation k = IntMap.adjust invertInstruction k instructions

countOnLoop :: Result -> Maybe Int
countOnLoop (Loop (c, _, _)) = Just c
countOnLoop _ = Nothing

countOnTerminated :: Result -> Maybe Int
countOnTerminated (Terminated (c, _, _)) = Just c
countOnTerminated _ = Nothing

findLoop :: [Instruction] -> Result
findLoop is = runProgram (0, 0, mkInstructionSet is)

findTerminated :: [Instruction] -> Maybe Result
findTerminated is =
  find terminated $
    runProgram . (0,0,) <$> mkVariations (IntMap.fromList $ zip [0 ..] is)

day08 :: IO ()
day08 = do
  (Right instructions) <- parseProgram <$> TIO.readFile "input/day08.txt"
  putStrLn $
    "Part 1: count was "
      <> maybe "_" show (countOnLoop $ findLoop instructions)
      <> " when the program went into a loop"

  putStrLn $
    "Part 2: count was "
      <> maybe "_" show (countOnTerminated =<< findTerminated instructions)
      <> " when the program terminated"
