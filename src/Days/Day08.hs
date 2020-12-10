module Days.Day08 (runDay) where

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
import Data.Void
import Control.Applicative ((<|>))
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = inst `sepBy` endOfLine
  where
    inst = do
      op <- choice [string "acc" $> Acc, string "jmp" $> Jmp, string "nop" $> Nop]
      string " "
      sgn <- char '-' $> (\x -> (- x)) <|> char '+' $> id
      arg <- decimal
      return (op, sgn arg)

------------ TYPES ------------
data Op = Acc | Jmp | Nop deriving (Show)

type Program = [(Op, Int)]

type Input = Program

type OutputA = Int

type OutputB = Int

-- pc, acc
type State = (Int, Int)

------------ PART A ------------
updateState :: (Op, Int) -> State -> State
updateState (Nop, _) state = (fst state + 1, snd state)
updateState (Acc, arg) state = (fst state + 1, snd state + arg)
updateState (Jmp, arg) state = (fst state + arg, snd state)

runval :: Program -> State -> [Int] -> Int
runval prog state exclude =
  let newState = updateState (prog !! fst state) state
   in if fst newState `elem` exclude
        then snd newState
        else runval prog newState (fst newState : exclude)

partA :: Input -> OutputA
partA inp = runval inp (0, 0) []

------------ PART B ------------
flipInst :: (Op, Int) -> (Op, Int)
flipInst (Nop, a) = (Jmp, a)
flipInst (Jmp, a) = (Nop, a)
flipInst (Acc, a) = (Acc, a)

flipProg' :: ([(Op, Int)], [(Op, Int)]) -> Program
flipProg' (a, []) = a
flipProg' (a, b) = a ++ [flipInst (head b)] ++ tail b

flipProg :: Int -> Program -> Program
flipProg index prog = flipProg' (splitAt index prog)

runval' :: Program -> State -> [Int] -> Maybe Int
runval' prog state exclude =
  let newState = updateState (prog !! fst state) state
   in if fst newState >= length prog
        then Just (snd newState)
        else
          if fst newState `elem` exclude
            then Nothing
            else runval' prog newState (fst newState : exclude)

partB :: Input -> OutputB
partB prog = head $ catMaybes [runval' (flipProg x prog) (0, 0) [] | x <- [-1 .. length prog - 1]]
