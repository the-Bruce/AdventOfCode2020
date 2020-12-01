module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA inp = head [x * y | x <- inp, y <- inp, x + y == 2020]

------------ PART B ------------
partB :: Input -> OutputB
partB inp = head [x * y * z | x <- inp, y <- inp, z <- inp, x + y + z == 2020]
