module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative.Combinators ((<|>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = boarding `sepBy` endOfLine
  where
    boarding = do
      row <- many1 (char 'F' <|> char 'B')
      col <- many1 (char 'L' <|> char 'R')
      return (row, col)

------------ TYPES ------------
type Input = [(String, String)]

type OutputA = Int

type OutputB = Int

type Range = (Int, Int)

------------ PART A ------------

splitLow :: Range -> Range
splitLow (a, b) = (a, a + ((b - a) `div` 2))

splitHigh :: Range -> Range
splitHigh (a, b) = (a + ((b - a + 1) `div` 2), b)

splitIf :: Char -> (Range -> Range)
splitIf 'B' = splitHigh
splitIf 'F' = splitLow
splitIf 'R' = splitHigh
splitIf 'L' = splitLow

pos :: (String, String) -> (Int, Int)
pos (row, col) = (fst $ foldl (flip splitIf) (0, 127) row, fst $ foldl (flip splitIf) (0,7) col)

pairID :: (Int, Int) -> Int
pairID (a,b) = a*8+b

seats :: [(String, String)] -> [Int]
seats = map (pairID . pos)

partA :: Input -> OutputA
partA inp = maximum $ seats inp

------------ PART B ------------
partB :: Input -> OutputB
partB inp = head [x+1| x <- (seats inp), y <- (seats inp), y-x==2, x+1 `notElem` (seats inp)]
