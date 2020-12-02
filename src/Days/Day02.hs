module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
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
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ruleAndPassword `sepBy` endOfLine
  where
    ruleAndPassword = do
      minv <- decimal
      char '-'
      maxv <- decimal
      skipSpace
      given <- letter
      asciiCI ": "
      password <- many1 letter
      return Password {minv = minv, maxv = maxv, given = given, password = password}

------------ TYPES ------------
data Password = Password {minv :: Int, maxv :: Int, given :: Char, password :: String} deriving (Show)

type Input = [Password]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
passvalid1 :: Password -> Bool
passvalid1 pass =
  let len = length (filter ((\p x -> x == given p) pass) (password pass))
   in len >= minv pass && len <= maxv pass

partA :: Input -> OutputA
partA inp = length $ filter passvalid1 inp

------------ PART B ------------
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

passvalid2 :: Password -> Bool
passvalid2 pass = ((password pass !! (minv pass - 1)) == given pass) `xor` ((password pass !! (maxv pass - 1)) == given pass)

partB :: Input -> OutputB
partB inp = length $ filter passvalid2 inp
