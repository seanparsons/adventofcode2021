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

type Entry = ([String], [String])

testInput :: [String]
testInput = [
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
  "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
  "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
  "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
  "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
  "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
  "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
  "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
  "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
  "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]

parseSplit :: String -> String -> IO (String, String)
parseSplit toSplitOn text = do
  case (splitOn toSplitOn text) of
    first : second : [] -> pure (first, second)
    _ -> fail ("Could not parse split: " <> text)

parseEntry :: String -> IO Entry
parseEntry entry = do
  (input, output) <- parseSplit " | " entry
  let inputEntries = splitOn " " input
  let outputEntries = splitOn " " output
  pure (inputEntries, outputEntries)

is1478Output :: String -> Bool
is1478Output output =
  case length output of
    2 -> True -- 1
    4 -> True -- 4
    3 -> True -- 7
    7 -> True -- 8
    _ -> False

count1478 :: Entry -> Int
count1478 (_, outputEntries) = length $ filter is1478Output outputEntries

countAll :: [Entry] -> Int
countAll entries = getSum $ foldMap (\e -> Sum $ count1478 e) entries

getEntryWithPredicate :: (String -> Bool) -> String -> [String] -> IO String
getEntryWithPredicate pred errorMessage entries = maybe (fail errorMessage) pure $ find pred entries

getEntryWithLength :: Int -> [String] -> IO String
getEntryWithLength l entries = getEntryWithPredicate (\e -> length e == l) ("Could not find entry with length " <> show l) entries

getCharacterCounts :: [String] -> HashMap Char Int
getCharacterCounts entries =
  let allChars = toListOf (traverse . traverse) entries
      sumMap = foldl' (\working -> \c -> alter (<> (Just $ Sum 1)) c working) mempty allChars
   in fmap getSum sumMap

findFMapping :: HashMap Char Int -> IO Char
findFMapping charCounts =
  maybe (fail "Could not find mapping for 'f'.") pure $ getFirst $ foldMapWithKey (\k -> \v -> if v == 9 then (First $ Just k) else (First Nothing)) charCounts

-- 2 segments: 1
-- 3 segments: 7
-- 4 segments: 4
-- 5 segments: 2, 3, 5
-- 6 segments: 0, 6, 9
-- 7 segments: 8

zeroDigits :: HS.HashSet Char
zeroDigits = HS.fromList ['a', 'b', 'c', 'e', 'f', 'g']

oneDigits :: HS.HashSet Char
oneDigits = HS.fromList ['c', 'f']

twoDigits :: HS.HashSet Char
twoDigits = HS.fromList ['a', 'c', 'd', 'e', 'g']

threeDigits :: HS.HashSet Char
threeDigits = HS.fromList ['a', 'c', 'd', 'f', 'g']

fourDigits :: HS.HashSet Char
fourDigits = HS.fromList ['b', 'c', 'd', 'f']

fiveDigits :: HS.HashSet Char
fiveDigits = HS.fromList ['a', 'b', 'd', 'f', 'g']

sixDigits :: HS.HashSet Char
sixDigits = HS.fromList ['a', 'b', 'd', 'e', 'f', 'g']

sevenDigits :: HS.HashSet Char
sevenDigits = HS.fromList ['a', 'c', 'f']

eightDigits :: HS.HashSet Char
eightDigits = HS.fromList ['a'..'g']

nineDigits :: HS.HashSet Char
nineDigits = HS.fromList ['a', 'b', 'c', 'd', 'f', 'g']

findNumber :: HS.HashSet Char -> IO Int
findNumber chars
  | chars == zeroDigits = pure 0
  | chars == oneDigits = pure 1
  | chars == twoDigits = pure 2
  | chars == threeDigits = pure 3
  | chars == fourDigits = pure 4
  | chars == fiveDigits = pure 5
  | chars == sixDigits = pure 6
  | chars == sevenDigits = pure 7
  | chars == eightDigits = pure 8
  | chars == nineDigits = pure 9
  | otherwise = fail ("Could not find number for " <> show chars)

determineNumber :: HashMap Char Char -> String -> IO Int
determineNumber mappings numberChars = do
  mapped <- traverse (\c -> maybe (fail "No mapping.") pure $ lookup c mappings) numberChars
  let asSet = HS.fromList mapped
  findNumber asSet

--solveEntry :: Entry -> IO (Char, Char, Char, Char, String, String)
solveEntry (inputEntries, outputEntries) = do
  one <- getEntryWithLength 2 inputEntries
  seven <- getEntryWithLength 3 inputEntries
  four <- getEntryWithLength 4 inputEntries
  let charCounts = getCharacterCounts inputEntries
  fMapping <- findFMapping charCounts
  cMapping <- maybe (fail "Could not find mapping for 'c'.") pure $ firstOf (traverse . filtered (/= fMapping)) one
  aMapping <- maybe (fail "Could not find mapping for 'a'.") pure $ firstOf (traverse . filtered (\l -> l /= fMapping && l /= cMapping)) seven
  six <- getEntryWithPredicate (\e -> length e == 6 && elem aMapping e && elem fMapping e && notElem cMapping e) "Could not find 6." inputEntries
  let sixSet = HS.fromList six
  five <- getEntryWithPredicate (\e -> length e == 5 && (HS.isSubsetOf (HS.fromList e) sixSet)) "Could not find 7." inputEntries
  eMapping <- maybe (fail "Could not find mapping for 'e'.") pure $ firstOf traverse $ HS.toList $ HS.difference sixSet $ HS.fromList five
  nine <- getEntryWithPredicate (\e -> length e == 6 && notElem eMapping e) "Could not find 9." inputEntries
  zero <- getEntryWithPredicate (\e -> length e == 6 && HS.isSubsetOf (HS.fromList [fMapping, cMapping, aMapping, eMapping]) (HS.fromList e)) "Could not find 0." inputEntries
  dMapping <- maybe (fail "Could not find mapping for 'd'.") pure $ firstOf traverse $ HS.toList $ HS.difference (HS.fromList "abcdefg") (HS.fromList zero)
  bMapping <- maybe (fail "Could not find mapping for 'b'.") pure $ firstOf traverse $ HS.toList $ HS.difference (HS.fromList four) (HS.fromList [cMapping, dMapping, fMapping])
  gMapping <- maybe (fail "Could not find mapping for 'g'.") pure $ firstOf traverse $ HS.toList $ HS.difference (HS.fromList "abcdefg") (HS.fromList [fMapping, cMapping, aMapping, eMapping, dMapping, bMapping])
  let mappings = fromList [(aMapping, 'a'), (bMapping, 'b'), (cMapping, 'c'), (dMapping, 'd'), (eMapping, 'e'), (fMapping, 'f'), (gMapping, 'g')]
  outputNumbers <- traverse (determineNumber mappings) outputEntries
  pure $ foldMap (\(n, p) -> Sum (n * p)) $ zip outputNumbers [1000, 100, 10, 1]

runDay8 :: IO ()
runDay8 = do
  putStrLn "Day 8"
  fileContent <- readFile "./day8input.txt"
  let fileLines = lines fileContent
  parsedInput <- traverse parseEntry fileLines
  result <- foldMap solveEntry parsedInput
  print result
