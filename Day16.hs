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

charMapping :: HashMap Char [Bool]
charMapping = fromList
  [ ('0', [False, False, False, False])
  , ('1', [False, False, False, True])
  , ('2', [False, False, True, False])
  , ('3', [False, False, True, True])
  , ('4', [False, True, False, False])
  , ('5', [False, True, False, True])
  , ('6', [False, True, True, False])
  , ('7', [False, True, True, True])
  , ('8', [True, False, False, False])
  , ('9', [True, False, False, True])
  , ('A', [True, False, True, False])
  , ('B', [True, False, True, True])
  , ('C', [True, True, False, False])
  , ('D', [True, True, False, True])
  , ('E', [True, True, True, False])
  , ('F', [True, True, True, True])
  ]

flattenOutToBits :: String -> [Bool]
flattenOutToBits [] = []
flattenOutToBits (first : rest) = findWithDefault [] first charMapping <> flattenOutToBits rest

type Version = Int

data Packet = Literal Version Int
            | Operator Version Int [Packet]
            deriving (Eq, Show)

bitsToInt :: [Bool] -> Int
bitsToInt bits =
  let multipliers = fmap (\n -> 2 ^ n) [0..]
      combined = zip (reverse bits) multipliers
      addIt (True, n) = Sum n
      addIt (False, _) = Sum 0
   in getSum $ foldMap addIt combined

parseLiteralBits :: [Bool] -> [Bool] -> IO ([Bool], [Bool])
parseLiteralBits valueBits (True : first : second : third : fourth : rest) = do
  parseLiteralBits (valueBits <> [first, second, third, fourth]) rest
parseLiteralBits valueBits (False : first : second : third : fourth : rest) = do
  let literalBits = valueBits <> [first, second, third, fourth]
  pure (literalBits, rest)
parseLiteralBits _ _ = do
  fail "Unexpected value found when attempting to parse literal bits."

parseLiteral :: Int -> [Bool] -> IO (Packet, [Bool])
parseLiteral version bits = do
  (literalBits, rest) <- parseLiteralBits [] bits
  let packet = Literal version (bitsToInt literalBits)
  pure (packet, rest)

parseOperator :: Int -> Int -> [Bool] -> IO (Packet, [Bool])
parseOperator version packetType (False : bits) = do -- Total length in bits.
  let (lengthBits, afterLength) = splitAt 15 bits
  unless (length lengthBits == 15) (fail "Too few bits for operator length.")
  let numberOfBits = bitsToInt lengthBits
  let (subPacketBits, afterPackets) = splitAt numberOfBits afterLength
  unless (length subPacketBits == numberOfBits) (fail "Too few bits loaded from operator length.")
  subPackets <- parsePacketsFromBits subPacketBits
  pure (Operator version packetType subPackets, afterPackets)
parseOperator version packetType (True : bits) = do -- Number of sub-packets.
  let (countBits, afterCount) = splitAt 11 bits
  unless (length countBits == 11) (fail "Too few bits for packet count.")
  let numberOfPackets = bitsToInt countBits
  (subPackets, rest) <- parsePacketsByCount numberOfPackets afterCount
  pure (Operator version packetType subPackets, rest)

parsePacketsByCount :: Int -> [Bool] -> IO ([Packet], [Bool])
parsePacketsByCount 0 bits = pure ([], bits)
parsePacketsByCount packetCount bits = do
  (packet, rest) <- parsePacket bits
  (otherPackets, finalRest) <- parsePacketsByCount (packetCount - 1) rest
  pure (packet : otherPackets, finalRest)

parsePacketsFromBits :: [Bool] -> IO [Packet]
parsePacketsFromBits [] = pure []
parsePacketsFromBits bits = do
  (packet, rest) <- parsePacket bits
  otherPackets <- parsePacketsFromBits rest
  pure (packet : otherPackets)

parsePacket :: [Bool] -> IO (Packet, [Bool])
parsePacket (first : second : third : fourth : fifth : sixth : rest) = do
  let version = bitsToInt [first, second, third]
  let packetType = bitsToInt [fourth, fifth, sixth]
  case packetType of
    4 -> parseLiteral version rest
    _ -> parseOperator version packetType rest
parsePacket bits = fail ("Unexpected packet remainder: " <> show bits)

packetVersionCount :: Packet -> Int
packetVersionCount (Literal version _) = version
packetVersionCount (Operator version _ subPackets) = version + versionCounts subPackets

versionCounts :: [Packet] -> Int
versionCounts packets = foldl' (\working -> \packet -> working + packetVersionCount packet) 0 packets

processPacket :: Packet -> IO Int
processPacket (Literal _ value) = pure value
processPacket (Operator _ 0 subPackets) = fmap getSum $ foldMap (\p -> fmap Sum $ processPacket p) subPackets
processPacket (Operator _ 1 subPackets) = fmap getProduct $ foldMap (\p -> fmap Product $ processPacket p) subPackets
processPacket (Operator _ 2 subPackets) = fmap getMin $ foldMap (\p -> fmap Min $ processPacket p) subPackets
processPacket (Operator _ 3 subPackets) = fmap getMax $ foldMap (\p -> fmap Max $ processPacket p) subPackets
processPacket (Operator _ 5 [packet1, packet2]) = do
  packet1Result <- processPacket packet1
  packet2Result <- processPacket packet2
  pure $ if packet1Result > packet2Result then 1 else 0
processPacket (Operator _ 6 [packet1, packet2]) = do
  packet1Result <- processPacket packet1
  packet2Result <- processPacket packet2
  pure $ if packet1Result < packet2Result then 1 else 0
processPacket (Operator _ 7 [packet1, packet2]) = do
  packet1Result <- processPacket packet1
  packet2Result <- processPacket packet2
  pure $ if packet1Result == packet2Result then 1 else 0
processPacket packet = fail ("Unexpected value: " <> show packet)

runDay16 :: IO ()
runDay16 = do
  putStrLn "Day 16"
  fileContent <- readFile "./day16input.txt"
  let fileLines = lines fileContent
  line <- case fileLines of
            (first : []) -> pure first
            _ -> fail "Unexpected content."
  let bits = flattenOutToBits line
  (packet, _) <- parsePacket bits
  print $ packetVersionCount packet
  packetResult <- processPacket packet
  print packetResult

