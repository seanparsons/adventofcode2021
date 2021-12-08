import Control.Lens
import System.IO
import Text.Read
import Data.Maybe
import Control.Monad
import Control.Monad.Fail
import Data.Monoid
import Data.Foldable
import Data.List hiding (insert)
import Data.List.Split
import Debug.Trace
import Data.HashMap.Strict hiding (foldl')

type World = HashMap Int Int

initialState :: [Int]
--initialState = [3,4,3,1,2]
initialState = [5,1,1,5,4,2,1,2,1,2,2,1,1,1,4,2,2,4,1,1,1,1,1,4,1,1,1,1,1,5,3,1,4,1,1,1,1,1,4,1,5,1,1,1,4,1,2,2,3,1,5,1,1,5,1,1,5,4,1,1,1,4,3,1,1,1,3,1,5,5,1,1,1,1,5,3,2,1,2,3,1,5,1,1,4,1,1,2,1,5,1,1,1,1,5,4,5,1,3,1,3,3,5,5,1,3,1,5,3,1,1,4,2,3,3,1,2,4,1,1,1,1,1,1,1,2,1,1,4,1,3,2,5,2,1,1,1,4,2,1,1,1,4,2,4,1,1,1,1,4,1,3,5,5,1,2,1,3,1,1,4,1,1,1,1,2,1,1,4,2,3,1,1,1,1,1,1,1,4,5,1,1,3,1,1,2,1,1,1,5,1,1,1,1,1,3,2,1,2,4,5,1,5,4,1,1,3,1,1,5,5,1,3,1,1,1,1,4,4,2,1,2,1,1,5,1,1,4,5,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,4,2,1,1,1,2,5,1,4,1,1,1,4,1,1,5,4,4,3,1,1,4,5,1,1,3,5,3,1,2,5,3,4,1,3,5,4,1,3,1,5,1,4,1,1,4,2,1,1,1,3,2,1,1,4]

insertOrUpdate :: Int -> Maybe Int -> Maybe Int
insertOrUpdate toAdd Nothing = Just toAdd
insertOrUpdate toAdd (Just count) = Just (count + toAdd)

worldStep :: World -> Int -> Int -> World
worldStep workingWorld 0 fishCount = alter (insertOrUpdate fishCount) 6 $ alter (insertOrUpdate fishCount) 8 workingWorld
worldStep workingWorld fishAge fishCount = alter (insertOrUpdate fishCount) (fishAge - 1) workingWorld

worldDay :: World -> World
worldDay world = foldlWithKey' worldStep mempty world

runFn :: Int -> (a -> a) -> a -> a
runFn counter fn starting
  | counter <= 0 = starting
  | otherwise = runFn (counter - 1) fn $ fn starting

stateToWorld :: [Int] -> World
stateToWorld state = foldl' (\world -> \fishAge -> alter (insertOrUpdate 1) fishAge world) mempty initialState

runWorld :: World
runWorld = runFn 256 worldDay $ stateToWorld initialState

runDay6 :: IO ()
runDay6 = do
  putStrLn "Day 6"
  let result = runWorld
  print $ sum $ elems result

