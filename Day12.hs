import Control.Lens
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad
import Control.Monad.Fail
import Data.Semigroup hiding (First, getFirst)
import Data.Monoid
import Data.Foldable
import Data.List hiding (insert, lookup)
import Data.List.Split
import Debug.Trace
import Data.HashMap.Strict hiding (foldl', filter)
import qualified Data.HashSet as HS
import Prelude hiding (lookup)
import qualified Data.Vector as V
import Control.Monad.Trans.Writer.CPS
import Data.Char(isLower)

type Segment = (String, String)

parseSplit :: String -> String -> IO (String, String)
parseSplit toSplitOn text = do
  case (splitOn toSplitOn text) of
    first : second : [] -> pure (first, second)
    _ -> fail ("Could not parse split: " <> text)

parseEntry :: String -> IO [Segment]
parseEntry entry = do
  (first, second) <- parseSplit "-" entry
  let segments = [(first, second), (second, first)]
  pure $ filter (\(first, second) -> first /= "end" && second /= "start") segments

parseInput :: [String] -> IO [Segment]
parseInput entries = do
  fmap join $ traverse parseEntry entries

isLowercaseString :: String -> Bool
isLowercaseString string = all isLower string

isUppercaseString :: String -> Bool
isUppercaseString string = not $ isLowercaseString string

testInput :: [String]
testInput =
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

findNextStepsPartOne :: [Segment] -> [String] -> [String]
findNextStepsPartOne world [] = ["start"]
findNextStepsPartOne world soFar@(end : _) =
  let isConnectedToEnd (start, _) = start == end
      notDuplicateLowercase next = isUppercaseString next || (not $ elem next soFar)
   in toListOf (traverse . filtered isConnectedToEnd . _2 . filtered notDuplicateLowercase) world

walkPathsPartOne :: [Segment] -> [String] -> Writer [[String]] ()
walkPathsPartOne world [] = walkPathsPartOne world $ findNextStepsPartOne world []
walkPathsPartOne world soFar@(end : _) = do
  when (end == "end") $ tell [soFar]
  let nextSteps = findNextStepsPartOne world soFar
  traverse_ (\step -> walkPathsPartOne world (step : soFar)) nextSteps

getPathsPartOne :: [Segment] -> [[String]]
getPathsPartOne world = fmap reverse $ snd $ runWriter $ walkPathsPartOne world []


findNextStepsPartTwo :: [Segment] -> Bool -> [String] -> [(Bool, String)]
findNextStepsPartTwo world _ [] = [(False, "start")]
findNextStepsPartTwo world smallDoubleVisited soFar@(end : _) =
  let isConnectedToEnd (start, _) = start == end
      notDuplicateLowercase next = isUppercaseString next || not smallDoubleVisited || (not $ elem next soFar)
   in fmap (\s -> (isLowercaseString s && elem s soFar, s)) $ toListOf (traverse . filtered isConnectedToEnd . _2 . filtered notDuplicateLowercase) world

walkPathsPartTwo :: [Segment] -> Bool -> [String] -> Writer (Sum Int) ()
walkPathsPartTwo world smallDoubleVisited soFar = do
  when (firstOf traverse soFar == Just "end") $ tell (Sum 1)
  let nextSteps = findNextStepsPartTwo world smallDoubleVisited soFar
  traverse_ (\(newSmallDoubleVisited, step) -> walkPathsPartTwo world (smallDoubleVisited || newSmallDoubleVisited) (step : soFar)) nextSteps

getPathsPartTwo :: [Segment] -> Int
getPathsPartTwo world = getSum $ snd $ runWriter $ walkPathsPartTwo world False []


printPath :: [String] -> IO ()
printPath path = do
  putStrLn $ intercalate "," path

runDay12 :: IO ()
runDay12 = do
  putStrLn "Day 12"
  fileContent <- readFile "./day12input.txt"
  let fileLines = lines fileContent
  parsedInput <- parseInput fileLines
  --print parsedInput
  putStrLn "Part 1"
  let partOneResult = getPathsPartOne parsedInput
  print $ length partOneResult
  putStrLn "Part 2"
  let partTwoResult = getPathsPartTwo parsedInput
  print partTwoResult
