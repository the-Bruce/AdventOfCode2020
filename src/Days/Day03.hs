module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map, findWithDefault)
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

import Util.Parsers (coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (flip lookup [('#', 1), ('.', 0)]) 0

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp =
  let (width, len) = (maximum (map fst (Map.keys inp)) + 1, maximum (map snd (Map.keys inp)))
   in sum [findWithDefault 0 ((x * 3) `mod` width, x) inp | x <- [0 .. len]]

------------ PART B ------------
partB :: Input -> OutputB
partB inp =
  let (width, len) = (maximum (map fst (Map.keys inp)) + 1, maximum (map snd (Map.keys inp)))
   in product [ sum [findWithDefault 0 ((x `div` stepx * stepy) `mod` width, x) inp | x <- [0, stepx .. len]]
        | (stepx, stepy) <- [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
      ]
