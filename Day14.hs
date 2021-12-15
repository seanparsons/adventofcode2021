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
import Data.HashMap.Strict hiding (foldl', filter)
import qualified Data.HashSet as HS
import Prelude hiding (lookup)
import qualified Data.Vector as V
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Strict
import Data.Char(isLower)

type InsertionRules = HashMap (Char, Char) Char

type ParsingInstructions = (First String, InsertionRules)

type Instructions = (String, InsertionRules)

testInput :: [String]
testInput =
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

parseSplit :: String -> String -> IO (String, String)
parseSplit toSplitOn text = do
  case (splitOn toSplitOn text) of
    first : second : [] -> pure (first, second)
    _ -> fail ("Could not parse split: " <> text)

parseInt :: String -> IO Int
parseInt possibleInt = maybe (fail "Could not parse number.") pure $ readMaybe possibleInt 

parsePairInsertion :: String -> IO ParsingInstructions
parsePairInsertion entry = do
  (adjacentElements, toInsert) <- parseSplit " -> " entry
  charToInsert <- case toInsert of
                       (c : []) -> pure c
                       otherwise -> fail ("Invalid toInsert: " <> toInsert)
  (firstChar, secondChar) <- case adjacentElements of
                                  (f : s : []) -> pure (f, s)
                                  otherwise -> fail ("Invalid adjacentElements: " <> adjacentElements)
  pure (First Nothing, singleton (firstChar, secondChar) charToInsert)

parseTemplate :: String -> IO ParsingInstructions
parseTemplate entry = pure (First $ Just entry, mempty)

parseEntry :: String -> IO ParsingInstructions 
parseEntry "" = mempty
parseEntry entry@(_ : _ : ' ' : '-' : '>' : _) = parsePairInsertion entry
parseEntry entry = parseTemplate entry

parseInstructions :: [String] -> IO Instructions
parseInstructions entries = do
  (First possibleTemplate, rules) <- foldMap parseEntry entries
  polymerTemplate <- maybe (fail "No polymer template provided.") pure possibleTemplate
  pure (polymerTemplate, rules)
  
calculateResult :: HashMap Char (Sum Int) -> Int
calculateResult counts =
  let (First possibleLowest, Last possibleHighest) = foldMap (\n -> (First $ Just n, Last $ Just n)) $ fmap snd $ sortOn snd $ toList $ fmap getSum counts
      combined = fmap Sum possibleHighest <> fmap (\n -> Sum (-n)) possibleLowest
   in fromMaybe 0 $ fmap getSum combined

type CountMap = HashMap Char (Sum Int)

type Cache = HashMap (Char, Char, Int) CountMap

type CacheState = State Cache

addToCount :: Maybe (Sum Int) -> Maybe (Sum Int)
addToCount current = current <> (Just $ Sum 1)

reduceCount :: Maybe (Sum Int) -> Maybe (Sum Int)
reduceCount current = current <> (Just $ Sum (-1))

runInsertion :: InsertionRules -> Char -> Char -> Int -> CacheState CountMap
runInsertion rules first second recurseCount
  | recurseCount < 0 = pure mempty
  | recurseCount == 0 = do
    pure $ alter addToCount first $ alter addToCount second mempty
  | otherwise = do
    let triple = (first, second, recurseCount)
    existingResult <- gets $ lookup triple
    case existingResult of
      Just existing -> pure existing
      Nothing -> do
        let possibleRuleResult = lookup (first, second) rules
        countResult <- case possibleRuleResult of
          Nothing -> pure $ alter addToCount first $ alter addToCount second mempty
          Just ruleResult -> do
            leftLookup <- runInsertion rules first ruleResult (recurseCount - 1)
            rightLookup <- runInsertion rules ruleResult second (recurseCount - 1)
            pure $ alter reduceCount ruleResult $ unionWith (<>) leftLookup rightLookup
        modify' (insert triple countResult)
        pure countResult

pairsOfList :: [a] -> [(a, a)]
pairsOfList [] = []
pairsOfList (_ : []) = []
pairsOfList (first : second : rest) = (first, second) : pairsOfList (second : rest)

runInsertions :: InsertionRules -> Int -> String -> CacheState CountMap
runInsertions rules recurseCount template = do
  let characterPairs = pairsOfList template
  resultSoFar <- foldlM (\working -> \(first, second) -> fmap (\counts -> unionWith (<>) working counts) $ runInsertion rules first second recurseCount) mempty characterPairs
  let charactersToDropByOne = drop 1 $ init template
  pure $ foldl' (\working -> \c -> alter reduceCount c working) resultSoFar charactersToDropByOne

runDay14 :: IO ()
runDay14 = do
  fileContent <- readFile "./day14input.txt"
  let fileLines = lines fileContent
  (polymerTemplate, rules) <- parseInstructions fileLines
  let (testResult, part1Result, part2Result) = (flip evalState) mempty $ do
            test <- runInsertions rules 1 polymerTemplate
            part1 <- runInsertions rules 10 polymerTemplate
            part2 <- runInsertions rules 40 polymerTemplate
            pure (test, part1, part2)
  print testResult
  putStrLn "Part 1"
  print $ calculateResult part1Result
  putStrLn "Part 2"
  print $ calculateResult part2Result
