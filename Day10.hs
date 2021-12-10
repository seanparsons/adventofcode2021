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
import Data.HashMap.Strict hiding (foldl', filter, mapMaybe)
import qualified Data.HashSet as HS
import Prelude hiding (lookup)

testInput :: [String]
testInput =
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

partOneGetLineScore :: String -> String -> Int
partOneGetLineScore _ [] = 0
partOneGetLineScore ('(' : openingRest) (')' : closingRest) = partOneGetLineScore openingRest closingRest
partOneGetLineScore ('[' : openingRest) (']' : closingRest) = partOneGetLineScore openingRest closingRest
partOneGetLineScore ('{' : openingRest) ('}' : closingRest) = partOneGetLineScore openingRest closingRest
partOneGetLineScore ('<' : openingRest) ('>' : closingRest) = partOneGetLineScore openingRest closingRest
partOneGetLineScore opening ('(' : closingRest) = partOneGetLineScore ('(' : opening) closingRest
partOneGetLineScore opening ('[' : closingRest) = partOneGetLineScore ('[' : opening) closingRest
partOneGetLineScore opening ('{' : closingRest) = partOneGetLineScore ('{' : opening) closingRest
partOneGetLineScore opening ('<' : closingRest) = partOneGetLineScore ('<' : opening) closingRest
partOneGetLineScore _ (')' : _) = 3
partOneGetLineScore _ (']' : _) = 57
partOneGetLineScore _ ('}' : _) = 1197
partOneGetLineScore _ ('>' : _) = 25137
partOneGetLineScore _ _ = 0

charScore :: Char -> Int
charScore '(' = 1
charScore '[' = 2
charScore '{' = 3
charScore '<' = 4
charScore _ = 0

partTwoErrorScore :: String -> Int
partTwoErrorScore = foldl' (\working -> \c -> (working * 5) + charScore c) 0

partTwoGetLineScore :: String -> String -> Maybe Int
partTwoGetLineScore opening [] = Just $ partTwoErrorScore opening
partTwoGetLineScore ('(' : openingRest) (')' : closingRest) = partTwoGetLineScore openingRest closingRest
partTwoGetLineScore ('[' : openingRest) (']' : closingRest) = partTwoGetLineScore openingRest closingRest
partTwoGetLineScore ('{' : openingRest) ('}' : closingRest) = partTwoGetLineScore openingRest closingRest
partTwoGetLineScore ('<' : openingRest) ('>' : closingRest) = partTwoGetLineScore openingRest closingRest
partTwoGetLineScore opening ('(' : closingRest) = partTwoGetLineScore ('(' : opening) closingRest
partTwoGetLineScore opening ('[' : closingRest) = partTwoGetLineScore ('[' : opening) closingRest
partTwoGetLineScore opening ('{' : closingRest) = partTwoGetLineScore ('{' : opening) closingRest
partTwoGetLineScore opening ('<' : closingRest) = partTwoGetLineScore ('<' : opening) closingRest
partTwoGetLineScore _ _ = Nothing

runDay10 :: IO ()
runDay10 = do
  fileContent <- readFile "./day10input.txt"
  let fileLines = lines fileContent
  let partOneResult = getSum $ foldMap (\input -> Sum $ partOneGetLineScore [] input) fileLines
  print partOneResult
  let partTwoScores = sort $ mapMaybe (partTwoGetLineScore []) fileLines 
  print $ length partTwoScores
  print partTwoScores
  let partTwoResult = partTwoScores !! (length partTwoScores `div` 2)
  print partTwoResult
