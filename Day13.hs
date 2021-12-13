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

data Dimension = X | Y deriving (Eq, Show)

type Paper = HS.HashSet (Int, Int)

type ParsedInput = ([(Int, Int)], [(Dimension, Int)])

testInput :: [String]
testInput =
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"
  ]

parseSplit :: String -> String -> IO (String, String)
parseSplit toSplitOn text = do
  case (splitOn toSplitOn text) of
    first : second : [] -> pure (first, second)
    _ -> fail ("Could not parse split: " <> text)

parseInt :: String -> IO Int
parseInt possibleInt = maybe (fail "Could not parse number.") pure $ readMaybe possibleInt 

parsePair :: String -> IO (Int, Int)
parsePair text = do
  (firstText, secondText) <- parseSplit "," text
  first <- parseInt firstText
  second <- parseInt secondText
  pure (first, second)

parseDimension :: String -> IO Dimension
parseDimension "x" = pure X
parseDimension "y" = pure Y
parseDimension text = fail ("Could not parse: " <> text)

parseFold :: String -> IO (Dimension, Int)
parseFold text = do
  (firstText, secondText) <- parseSplit "=" text
  second <- parseInt secondText
  first <- parseDimension firstText
  pure (first, second)

parseEntry :: String -> IO ParsedInput
parseEntry "" = pure ([], [])
parseEntry entry =
  let isFold = isPrefixOf "fold along " entry
      handlePair = fmap (\pair -> ([pair], [])) $ parsePair entry
      handleFold = fmap (\fold -> ([], [fold])) $ parseFold $ drop 11 entry
   in if isFold then handleFold else handlePair

removeAlongFold :: Dimension -> Int -> Paper -> Paper
removeAlongFold X coordinate paper = HS.filter (\(x, _) -> x /= coordinate) paper
removeAlongFold Y coordinate paper = HS.filter (\(_, y) -> y /= coordinate) paper

foldOverPaper :: Dimension -> Int -> Paper -> Paper
foldOverPaper X coordinate paper = HS.map (\(x, y) -> (if x > coordinate then coordinate - (x - coordinate) else x, y)) paper
foldOverPaper Y coordinate paper = HS.map (\(x, y) -> (x, if y > coordinate then coordinate - (y - coordinate) else y)) paper

foldPaper :: Dimension -> Int -> Paper -> Paper
foldPaper dimension coordinate paper = foldOverPaper dimension coordinate $ removeAlongFold dimension coordinate paper

day13Part1 :: ParsedInput -> Int
day13Part1 (dots, folds) =
  let paper = HS.fromList dots
      afterFolds = foldl' (\working -> \(dimension, coordinate) -> foldPaper dimension coordinate working) paper $ take 1 folds
   in HS.size afterFolds

makeLine :: Paper -> Int -> Int -> Int -> String
makeLine paper y xMin xMax = fmap (\exists -> if exists then '#' else '.') $ fmap (\x -> HS.member (x, y) paper) [xMin..xMax]

makeLines :: Paper -> (Min Int, Max Int, Min Int, Max Int) -> [String]
makeLines paper (Min xMin, Max xMax, Min yMin, Max yMax) = fmap (\y -> makeLine paper y xMin xMax) [yMin..yMax]

day13Part2 :: ParsedInput -> IO ()
day13Part2 (dots, folds) = do
  let paper = HS.fromList dots
  let afterFolds = foldl' (\working -> \(dimension, coordinate) -> foldPaper dimension coordinate working) paper folds
  let bounds = foldMap (\(x, y) -> (Min x, Max x, Min y, Max y)) afterFolds
  let paperLines = makeLines afterFolds bounds
  traverse_ putStrLn paperLines

runDay13 :: IO ()
runDay13 = do
  fileContent <- readFile "./day13input.txt"
  let fileLines = lines fileContent
  parsedInput <- foldMap parseEntry fileLines
  let part1Result = day13Part1 parsedInput
  print part1Result
  day13Part2 parsedInput
