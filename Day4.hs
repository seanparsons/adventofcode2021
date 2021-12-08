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

type Board = [[(Int, Bool)]]

type Game = ([Int], [Board])

parseInt :: String -> IO Int
parseInt possibleInt = maybe (fail "Could not parse number.") pure $ readMaybe possibleInt 

parseBoardInt :: String -> IO (Int, Bool)
parseBoardInt possibleInt = fmap (\n -> (n, False)) $ parseInt possibleInt

parseBoard :: [String] -> IO Board
parseBoard lines = do
  let parseLine line = traverse parseBoardInt $ words line
   in traverse parseLine lines

parseBoards :: [String] -> IO [Board]
parseBoards ("" : l1 : l2 : l3 : l4 : l5 : rest) = do
  board <- parseBoard (l1 : l2 : l3 : l4 : l5 : [])
  remainingBoards <- parseBoards rest
  pure (board : remainingBoards)
parseBoards (_ : []) = fail "Unexhausted input."
parseBoards _ = pure []

parseRolls :: [String] -> IO ([Int], [String])
parseRolls (rollsLine : rest) = do
  rolls <- traverse parseInt $ splitOn "," rollsLine
  pure (rolls, rest)

parseFile :: String -> IO Game
parseFile filePath = do
  fileContent <- readFile filePath 
  let fileLines = lines fileContent
  (rolls, remainingLines) <- parseRolls fileLines
  boards <- parseBoards remainingLines
  pure (rolls, boards)

type WorkingResult = (Last (Int, Board), [Board])

setBoardNumber :: Int -> (Int, Bool) -> (Int, Bool)
setBoardNumber _ pair@(_, True) = pair
setBoardNumber bingoNumber pair@(boardNumber, False) = if bingoNumber == boardNumber then (boardNumber, True) else pair

addNumberToBoard :: Int -> Board -> Board
addNumberToBoard bingoNumber board = over (traverse . traverse) (setBoardNumber bingoNumber) board

winningBoard :: Board -> Bool
winningBoard board =
  let rowWin = any (all (\(_, b) -> b)) board
      columnWin = any (all (\(_, b) -> b)) $ transpose board 
   in rowWin || columnWin

foldBoard :: Int -> WorkingResult -> Board -> WorkingResult
foldBoard bingoNumber (latestWin, boardsSoFar) board =
  let updatedBoard = addNumberToBoard bingoNumber board
      alreadyWon = winningBoard board
      boardWon = winningBoard updatedBoard
      didWin = Last (Just (bingoNumber, updatedBoard))
      didNotWin = Last Nothing
   in (latestWin <> if not alreadyWon && boardWon then didWin else didNotWin, boardsSoFar <> [updatedBoard])

foldFn :: WorkingResult -> Int -> WorkingResult
foldFn (latestWin, boardState) bingoNumber = foldl' (foldBoard bingoNumber) (latestWin, []) boardState

unsetNumbers :: Board -> [Int]
unsetNumbers board = toListOf (traverse . traverse . filtered (\(_, b) -> not b) . _1) board

runDay4 :: IO ()
runDay4 = do
  (bingoNumbers, boards) <- parseFile "./day4input.txt"
  let (result, _) = foldl' foldFn (Last Nothing, boards) bingoNumbers
  print $ fmap (\(bingoNumber, board) -> (bingoNumber, unsetNumbers board)) $ getLast result
  let finalScore = fmap (\(bingoNumber, board) -> (sum $ unsetNumbers board) * bingoNumber) $ getLast result
  print finalScore

