import System.IO
import Text.Read
import Data.Maybe
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.List
import Debug.Trace

type BitCount = (Sum Int, Sum Int)

bitCount :: Char -> BitCount
bitCount '0' = (Sum 1, mempty)
bitCount '1' = (mempty, Sum 1)
bitCount _ = mempty

invertBit :: Char -> Char
invertBit '0' = '1'
invertBit '1' = '0'
invertBit c = c

bitsAsDecimal :: String -> Int
bitsAsDecimal bits = foldl' (\working -> \(c, bitPow) -> working + (if c == '1' then (2 ^ bitPow) else 0)) 0 $ zip (reverse bits) [0..]

popularBit :: BitCount -> Char
popularBit (Sum zero, Sum one) = if zero > one then '0' else '1'

popularBitPrioritiseZero :: BitCount -> Char
popularBitPrioritiseZero (Sum zero, Sum one) = if zero <= one then '0' else '1'

countsOfBits :: String -> BitCount
countsOfBits line = foldMap bitCount line

popularBits :: [String] -> String
popularBits bitLines = fmap popularBit $ fmap countsOfBits $ transpose bitLines

gamma :: [String] -> Int
gamma bitLines = bitsAsDecimal $ popularBits bitLines

epsilon :: [String] -> Int
epsilon bitLines = bitsAsDecimal $ fmap invertBit $ popularBits bitLines

firstMaybe :: [a] -> Maybe a
firstMaybe (first : _) = Just first
firstMaybe _ = Nothing

filterPopularIndex :: [String] -> Int -> (BitCount -> Char) -> [String]
filterPopularIndex lines@(line : []) _ _ = lines
filterPopularIndex bitLines index countChar =
  let popular = countChar $ foldMap bitCount $ fmap (\line -> line !! index) bitLines
   in filter (\line -> line !! index == popular) bitLines

oxygenRating :: [String] -> Int
oxygenRating bitLines =
  let lineLength = length (bitLines !! 0)
      possibleValues = foldl' (\working -> \index -> filterPopularIndex working index popularBit) bitLines [0..lineLength - 1]
   in maybe 0 bitsAsDecimal $ firstMaybe possibleValues

coRating :: [String] -> Int
coRating bitLines =
  let lineLength = length (bitLines !! 0)
      possibleValues = foldl' (\working -> \index -> filterPopularIndex working index popularBitPrioritiseZero) bitLines [0..lineLength - 1]
   in maybe 0 bitsAsDecimal $ firstMaybe possibleValues

runDay3 :: IO ()
runDay3 = do
  fileContent <- readFile "./day3input.txt"
  let fileLines = lines fileContent
  let g = gamma fileLines
  let e = epsilon fileLines
  print (g, e, g * e)
  let oxygen = oxygenRating fileLines
  let co = coRating fileLines
  print $ zip (popularBits fileLines) [0..]
  print (oxygen, co, oxygen * co)

