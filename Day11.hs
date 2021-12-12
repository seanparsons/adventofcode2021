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
import Control.Monad.Writer.Strict


type Octopus = Int

type World = V.Vector (V.Vector Octopus)

type Flashed = HS.HashSet (Int, Int)

testInput :: [String]
testInput =
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
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

shifts :: [(Int, Int)]
shifts = do
  x <- [-1, 0, 1]
  y <- if x == 0 then [-1, 1] else [-1, 0, 1]
  pure (x, y)

parseInput :: [String] -> IO World
parseInput entries = do
  parsedLists <- traverse (traverse  numberCharToInt) entries
  pure $ V.fromList $ fmap V.fromList parsedLists

applyFlash :: Flashed -> (Int, Int) -> Int -> Writer Flashed Int
applyFlash flashed pos energy
  | (not $ HS.member pos flashed) && energy > 9 = tell (HS.singleton pos) >> pure energy
  | otherwise = pure energy

positionsToBumpFromFlash :: Flashed -> [(Int, Int)]
positionsToBumpFromFlash flashed = do
  (x, y) <- HS.toList flashed
  (xShift, yShift) <- shifts
  pure (x + xShift, y + yShift)

runFlashStep :: Flashed -> World -> (Flashed, World)
runFlashStep flashed world =
  let flashLens = (itraversed <.> itraversed)
      (worldAfterFlash, newFlashed) = runWriter $ imapMOf flashLens (applyFlash flashed) world
      updatedWorld = foldl' (\working -> \(x, y) -> over (ix x . ix y) (+1) working) worldAfterFlash $ positionsToBumpFromFlash newFlashed
      updatedFlashed = newFlashed <> flashed
   in if HS.null newFlashed then (updatedFlashed, updatedWorld) else runFlashStep updatedFlashed updatedWorld

runStep :: World -> (Flashed, World)
runStep world =
  let incremented = over (traverse . traverse) (+1) world
      (flashed, updatedWorldBeforeResetsToZero) = runFlashStep mempty incremented
      updatedWorld = foldl' (\working -> \(x, y) -> set (ix x . ix y) 0 working) updatedWorldBeforeResetsToZero flashed
   in (flashed, updatedWorld)

runSteps :: Int -> World -> Int
runSteps stepNo startingWorld =
  let (flashed, updatedWorld) = runStep startingWorld
      everythingFlashed = HS.size flashed == (length $ toListOf (traverse . traverse) updatedWorld)
      nextStep = runSteps (stepNo + 1) updatedWorld
   in if everythingFlashed then stepNo else nextStep

runDay11 :: IO ()
runDay11 = do
  putStrLn "Day 11"
  fileContent <- readFile "./day11input.txt"
  let fileLines = lines fileContent
  world <- parseInput fileLines 
  print $ runSteps 1 world
