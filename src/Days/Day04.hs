module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` (string "\n\n")
  where
    passport =
      Map.fromList <$> do
        entry `sepBy` space -- Note that "space" parses both spaces and newlines (and tabs, etc)
    entry =
      (,)
        <$> (many1 letter)
        <*> ((char ':') *> many1 (letter <|> digit <|> char '#'))

------------ TYPES ------------
type Input = [Map String String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
validKeys :: Set String
validKeys = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

containsKeys :: Map String String -> Bool
containsKeys x = validKeys == intersection (Map.keysSet x) validKeys

partA :: Input -> OutputA
partA inp = length $ filter containsKeys inp

------------ PART B ------------
between :: Int -> Int -> Maybe Int -> Bool
between _ _ Nothing = False
between min' max' (Just val) = val >= min' && val <= max'

height :: String -> Bool
height inp
  | L.take 2 (reverse inp) == reverse "in" =
    between 59 76 (readMaybe (L.take (length inp - 2) inp) :: Maybe Int)
  | L.take 2 (reverse inp) == reverse "cm" =
    between 150 193 (readMaybe (L.take (length inp - 2) inp) :: Maybe Int)
  | otherwise = False
  
hair :: String -> Bool
hair inp = length inp == 7 && head inp == '#' && foldl (\x y -> x && (y `elem` ("0123456789abcdefABCDEF"::[Char]))) True (tail inp)

passp :: String -> Bool
passp inp = length inp == 9 && foldl (\x y -> x && (y `elem` ("0123456789"::[Char]))) True inp

false :: String -> Bool
false _ = False

true :: String -> Bool
true _ = True

validators :: Map String (String -> Bool)
validators =
  Map.fromList
    [ ("byr", \x -> between 1920 2002 (readMaybe x :: Maybe Int)),
      ("iyr", \x -> between 2010 2020 (readMaybe x :: Maybe Int)),
      ("eyr", \x -> between 2020 2030 (readMaybe x :: Maybe Int)),
      ("hgt", height),
      ("hcl", hair),
      ("ecl", \x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
      ("pid", passp),
      ("cid", true)
    ]

validateOne :: String -> String -> Bool
validateOne k = Map.findWithDefault false k validators

validate :: Map String String -> Bool
validate = Map.foldlWithKey (\ b k v -> b && validateOne k v) True 

partB :: Input -> OutputB
partB inp = length $ filter validate (filter containsKeys inp)
