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

testInput :: [String]
testInput =
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

numberCharToInt :: Char -> IO Int
numberCharToInt '0' = pure 0
numberCharToInt '1' = pure 1
numberCharToInt '2' = pure 2
numberCharToInt '3' = pure 3
numberCharToInt '4' = pure 4
numberCharToInt '5' = pure 5
numberCharToInt '6' = pure 6
numberCharToInt '7' = pure 7
numberCharToInt '8' = pure 8
numberCharToInt '9' = pure 9
numberCharToInt _ = fail "Not what we expected."

parseInput :: [String] -> IO [[Int]]
parseInput rows = do
  let parseRow row = traverse numberCharToInt row
  traverse parseRow rows

getHeight :: [[Int]] -> Int -> Int -> Maybe Int
getHeight heights row column = firstOf (ix row . ix column) heights

shifts :: [(Int, Int)]
shifts = [(-1, 0), (1, 0), (0, -1), (0, 1)]

isLowestHeight :: Int -> Maybe Int -> Bool
isLowestHeight height (Just adjacentHeight) = height < adjacentHeight
isLowestHeight _ Nothing = True

isLowest :: [[Int]] -> ((Int, Int), Int) -> Bool
isLowest heights ((row, column), height) =
  all (\(rowShift, colShift) -> isLowestHeight height $ getHeight heights (row + rowShift) (column + colShift)) shifts

findLowPointHeights :: [[Int]] -> [Int]
findLowPointHeights heights =
  toListOf ((itraversed <.> itraversed) . withIndex . filtered (isLowest heights) . _2) heights

type PosSet = HS.HashSet (Int, Int)

findNeighbouringBasinParts :: [[Int]] -> PosSet -> Int -> Int -> Int -> PosSet
findNeighbouringBasinParts heights alreadyExplored row column height =
  let neighbours = fmap (\(rowShift, colShift) -> (row + rowShift, column + colShift)) shifts
      notVisitedNeighbours = filter (\pos -> not $ HS.member pos alreadyExplored) neighbours
      neighboursWithHeight = foldl' (\working -> \(nR, nC) -> maybe working (\nH -> (nR, nC, nH) : working) $ getHeight heights nR nC) [] notVisitedNeighbours
      validNeighbours = filter (\(_, _, neighbourHeight) -> neighbourHeight < 9 && neighbourHeight > height) neighboursWithHeight
      neighbourBasins = fmap (\(nR, nC, nH) -> findNeighbouringBasinParts heights (HS.insert (nR, nC) alreadyExplored) nR nC nH) validNeighbours
   in HS.unions (alreadyExplored : neighbourBasins)

getBasin :: [[Int]] -> ((Int, Int), Int) -> PosSet
getBasin heights ((row, column), height) = findNeighbouringBasinParts heights (HS.singleton (row, column)) row column height

findLowPointBasins :: [[Int]] -> [PosSet]
findLowPointBasins heights =
  fmap (getBasin heights) $ toListOf ((itraversed <.> itraversed) . withIndex . filtered (isLowest heights)) heights

riskLevel :: [Int] -> Int
riskLevel heights = getSum $ foldMap (\h -> Sum (h + 1)) heights

runDay9 :: IO ()
runDay9 = do
  fileContent <- readFile "./day9input.txt"
  let fileLines = lines fileContent
  parsedInput <- parseInput fileLines
  let result = findLowPointBasins parsedInput
  let sortedResult = take 3 $ reverse $ sortOn HS.size result
  print $ getProduct $ foldMap (\b -> Product $ HS.size b) sortedResult
