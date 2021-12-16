import Control.Lens
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad
import Control.Monad.Fail
import Data.Semigroup hiding (First, getFirst, Last)
import Data.Monoid
import Data.Foldable hiding (toList)
import Data.List hiding (insert, lookup)
import Data.List.Split
import Debug.Trace
import Data.HashMap.Strict hiding (foldl', filter, foldr, mapMaybe)
import Prelude hiding (lookup)
import qualified Data.Vector as V
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Strict
import Data.Char(isLower)
import Data.Tuple
import Data.Function
import qualified Data.Set as S

type RiskLevel = Int

type World = V.Vector (V.Vector RiskLevel)

testInput :: [String]
testInput =
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581"
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

parseInput :: [String] -> IO World
parseInput entries = do
  parsedLists <- traverse (traverse numberCharToInt) entries
  pure $ V.fromList $ fmap V.fromList parsedLists

shifts :: [(Int, Int)]
shifts = [(-1, 0), (1, 0), (0, -1), (0, 1)]

getNeighbours :: World -> (Int, (Int, Int)) -> [(Int, (Int, Int))]
getNeighbours world (priorCost, (x, y)) = fmap (\(pos, cost) -> (priorCost + cost, pos)) $ mapMaybe (\(sX, sY) -> firstOf ((iix (x + sX) <.> iix (y + sY)) . withIndex) world) shifts

dijkstra
    :: (Ord cost, Ord node, Show cost, Show node)
    => ((cost, node) -> [(cost, node)]) -- ^ Where we can go from a node and the cost of that
    -> node                             -- ^ Where we want to get to
    -> (cost, node)                     -- ^ The start position
    -> Maybe (cost, node)               -- ^ Maybe the answer. Maybe it doesn't exist
dijkstra next target start = search mempty (S.singleton start)
    where
        search visited toBeVisited = case S.minView toBeVisited of
            Nothing -> Nothing
            Just ((cost, vertex), withoutVertex)
                | vertex == target            -> Just (cost, vertex)
                | vertex `S.member` visited   -> search visited withoutVertex
                | otherwise                   -> search visitedWithNode withNext
                where
                    visitedWithNode = S.insert vertex visited
                    withNext = foldr S.insert withoutVertex $ next (cost, vertex)

runDijkstra :: World -> IO Int
runDijkstra world = do
  startingNode <- maybe (fail "Could not find start.") pure $ fmap swap $ firstOf ((iix 0 <.> iix 0) . withIndex) world
  endNode <- maybe (fail "Could not find end.") pure $ lastOf ((itraversed <.> itraversed) . asIndex) world
  let result = dijkstra (getNeighbours world) endNode startingNode
  withStartingCost <- maybe (fail "Could not find result.") pure $ firstOf (_Just . _1) result
  pure (withStartingCost - fst startingNode)

largeWorld :: World -> World
largeWorld world =
  let repeatedHorizontally = fmap (\row -> V.concat $ fmap (\toAdd -> fmap (\n -> n + toAdd) row) [0..4]) world
      repeatedVertically = V.concat $ fmap (\toAdd -> fmap (\row -> fmap (\n -> n + toAdd) row) repeatedHorizontally) [0..4]
      reduceIfNecessary n = if n > 9 then reduceIfNecessary (n - 9) else n
   in over (traverse . traverse) reduceIfNecessary repeatedVertically

runDay15 :: IO ()
runDay15 = do
  putStrLn "Day 15"
  fileContent <- readFile "./day15input.txt"
  let fileLines = lines fileContent
  world <- parseInput fileLines
  day1Result <- runDijkstra world
  print day1Result
  let biggerWorld = largeWorld world
  day2Result <- runDijkstra biggerWorld
  print day2Result
