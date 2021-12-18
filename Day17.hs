{-# LANGUAGE RecordWildCards #-}

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

data TargetArea = TargetArea
                { xBounds :: (Int, Int)
                , yBounds :: (Int, Int)
                }
                deriving (Eq, Show)

data Probe = Probe
             { xVelocity :: Int
             , yVelocity :: Int
             , xPos      :: Int
             , yPos      :: Int
             , yMax      :: Int
             }
             deriving (Eq, Show)

updateXVelocity :: Int -> Int
updateXVelocity xVelocity
  | xVelocity < 0 = xVelocity + 1
  | xVelocity == 0 = 0
  | xVelocity > 0 = xVelocity - 1

runStep :: Probe -> Probe
runStep Probe{..} =
  let newXPos = xPos + xVelocity
      newYPos = yPos + yVelocity
      newXVelocity = updateXVelocity xVelocity
      newYVelocity = yVelocity - 1
      newYMax = max yMax newYPos
   in Probe { xPos = newXPos, yPos = newYPos, xVelocity = newXVelocity, yVelocity = newYVelocity, yMax = newYMax }

inBounds :: (Int, Int) -> Int -> Bool
inBounds (lower, upper) value = lower <= value && upper >= value

probeInTargetArea :: TargetArea -> Probe -> Bool
probeInTargetArea TargetArea{..} Probe{..} = inBounds xBounds xPos && inBounds yBounds yPos

runStepsRepeatedly :: TargetArea -> Int -> Probe -> Maybe (Max Int)
runStepsRepeatedly targetArea@TargetArea{..} stepsRemaining probe@Probe{..}
  | stepsRemaining <= 0 = Nothing
  | probeInTargetArea targetArea probe = Just $ Max yMax
  | xPos > snd xBounds = Nothing
  | yPos < fst yBounds && yPos < snd yBounds = Nothing
  | otherwise = runStepsRepeatedly targetArea (stepsRemaining - 1) (runStep probe)

debugRunStep :: Int -> TargetArea -> Probe -> IO Probe
debugRunStep count targetArea probe
  | count <= 0 = pure probe
  | otherwise = do
    print (probe, probeInTargetArea targetArea probe)
    let next = runStep probe
    debugRunStep (count - 1) targetArea next

runDay17 :: IO ()
runDay17 = do
  let targetArea = TargetArea { xBounds = (287, 309), yBounds = (-76, -48) }
  putStrLn "Day 17"
  let minXV = 0
  let maxXV = snd $ xBounds targetArea
  let maxYV = -(fst $ yBounds targetArea)
  let minYV = fst $ yBounds targetArea
  let initialVelocities = [minXV..maxXV] >>= (\xV -> fmap (\yV -> (xV, yV)) [minYV..maxYV])
  let maxYResult = foldMap (\(xV, yV) -> runStepsRepeatedly targetArea 200 (Probe xV yV 0 0 0)) initialVelocities
  print maxYResult
  let distinctInitialVelocities = length $ catMaybes $ fmap (\(xV, yV) -> fmap (\_ -> (xV, yV)) $ runStepsRepeatedly targetArea 200 (Probe xV yV 0 0 0)) initialVelocities
  print distinctInitialVelocities
