import Control.Lens
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad
import Control.Monad.Fail
import Data.Monoid
import Data.Foldable
import Data.List
import Data.List.Split
import Debug.Trace

type Pipe = (Int, Int, Int, Int)

type Position = (Int, Int)

parseSplit :: String -> String -> IO (String, String)
parseSplit toSplitOn text = do
  case (splitOn toSplitOn text) of
    first : second : [] -> pure (first, second)
    _ -> fail ("Could not parse split: " <> text)

parseInt :: String -> IO Int
parseInt possibleInt = maybe (fail "Could not parse number.") pure $ readMaybe possibleInt 

parsePair :: String -> IO Position
parsePair text = do
  (firstText, secondText) <- parseSplit "," text
  first <- parseInt firstText
  second <- parseInt secondText
  pure (first, second)

parsePipe :: String -> IO Pipe
parsePipe text = do
  let withoutSpaces = filter (/= ' ') text
  (startText, endText) <- parseSplit "->" withoutSpaces
  (x1, y1) <- parsePair startText
  (x2, y2) <- parsePair endText
  pure (x1, y1, x2, y2)

nextNumber :: Int -> Int -> Int
nextNumber from to
  | from < to = from + 1
  | from > to = from - 1
  | otherwise = from

parseFile :: String -> IO [Pipe]
parseFile filePath = do
  fileContent <- readFile filePath
  let fileLines = lines fileContent
  traverse parsePipe fileLines

pipeToPositions :: Pipe -> [Position]
pipeToPositions (x1, y1, x2, y2) =
  let nextX = nextNumber x1 x2
      nextY = nextNumber y1 y2
      isLastPos = x1 == x2 && y1 == y2
      lastPos = (x1, y1) : []
      nextPos = (x1, y1) : pipeToPositions (nextX, nextY, x2, y2)
   in if isLastPos then lastPos else nextPos

isHOrV :: Pipe -> Bool
isHOrV (x1, y1, x2, y2) = x1 == x2 || y1 == y2

getOverlaps :: [Pipe] -> [Position]
getOverlaps pipes =
  let positions = pipes >>= pipeToPositions
      grouped = group $ sort positions
      doOverlap = filter (\n -> length n > 1) grouped 
   in fmap (!! 0) doOverlap

runDay5 :: IO ()
runDay5 = do
  pipes <- parseFile "day5input.txt"
  let result = length $ getOverlaps pipes
  print result
