module Days.Day06 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = grp `sepBy` (string "\n\n")
  where
    grp = decl `sepBy` endOfLine
      where
        decl = many1 letter

------------ TYPES ------------
type Input = [[[Char]]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = sum $ map (Set.size . Set.fromList . concat) inp

------------ PART B ------------
letters :: Set Char
letters = Set.fromList "abcdefghijklmnopqrstuvwxyz"

partB :: Input -> OutputB
partB inp = sum $ map ((Set.size . foldl' Set.intersection letters) . map Set.fromList) inp
