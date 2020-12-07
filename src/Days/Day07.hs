module Days.Day07 (runDay) where

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
import Data.Functor (($>))
import Util.Parsers (around)
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> rule `sepBy` endOfLine
  where
    colour = do
      (adj, col) <- many1 letter `around` space
      return (adj ++ " " ++ col)
    rule = do
      container <- colour
      string " bags contain "
      rules <-
        choice
          [ string "no other bags" $> [],
            bag `sepBy1` (string ", ")
          ]
      char '.'
      return (container, rules)
    bag = do
      quant <- decimal
      space
      col <- colour
      space
      string "bags" <|> string "bag"
      return (quant, col)

------------ TYPES ------------
type Colour = String

-- We represent the rules as a map from colours to a list of int, colour pairs (representing the bags inside the key bag)
type BagRules = Map Colour [(Int, Colour)]

type RuleBags = Map Colour (Set Colour)

type Input = BagRules

type OutputA = Int

type OutputB = Int

------------ PART A ------------

parents :: BagRules -> Colour -> Set Colour
parents inp target = Map.keysSet $ Map.filter (\v -> target `elem` map snd v) inp

invert :: BagRules -> RuleBags
invert inp = Map.fromList [(col, parents inp col) | col <- Map.keys inp]

newCols :: RuleBags -> Set Colour -> Set Colour
newCols rules inp = Set.difference (Set.unions (mapMaybe (`Map.lookup` rules) (Set.toList inp))) inp

colourTransitiveClosure :: RuleBags -> Set Colour -> Set Colour
colourTransitiveClosure rules inp = 
  if inp == Set.empty
    then Set.empty
    else inp `Set.union` colourTransitiveClosure rules (newCols rules inp)

initial :: Set Colour
initial = Set.fromList ["shiny gold"]

partA :: Input -> OutputA
partA inp = Set.size $ Set.difference (colourTransitiveClosure (invert inp) initial) initial

------------ PART B ------------
evaluate :: BagRules -> Colour -> Int
evaluate rules col = 1 + sum (map (\ (quant, col') -> quant * evaluate rules col') (rules Map.! col))

partB :: Input -> OutputB
partB rules = evaluate rules "shiny gold" - 1
