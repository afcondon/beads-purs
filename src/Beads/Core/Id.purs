module Beads.Core.Id where

import Prelude

import Beads.Core.Types (IssueId(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, drop)
import Data.String.CodeUnits (length)
import Effect (Effect)
import Effect.Random (randomInt)

-- | Check if a string starts with a prefix
hasPrefix :: String -> String -> Boolean
hasPrefix prefix str = indexOf (Pattern prefix) str == Just 0

-- | Generate a new issue ID
-- | Uses random hex characters to create collision-resistant IDs
-- | Format: "bd-xxxx" where xxxx is 4+ hex characters
generateId :: Effect IssueId
generateId = do
  -- Generate 4 random hex characters
  -- In a real implementation, we'd use proper UUID/hash
  hex <- generateHex 4
  pure $ IssueId $ "bd-" <> hex

-- | Generate a random hex string of given length
generateHex :: Int -> Effect String
generateHex len = go len ""
  where
  go 0 acc = pure acc
  go n acc = do
    digit <- randomInt 0 15
    go (n - 1) (acc <> hexDigit digit)

  hexDigit :: Int -> String
  hexDigit = case _ of
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"

-- | Parse an issue ID from a string
-- | Accepts formats: "bd-xxxx", "xxxx"
parseId :: String -> Maybe IssueId
parseId s
  | hasPrefix "bd-" s = Just $ IssueId s
  | length s >= 4 && length s <= 8 = Just $ IssueId $ "bd-" <> s
  | otherwise = Nothing

-- | Extract the hash portion of an ID
-- | "bd-a1b2" -> "a1b2"
getIdHash :: IssueId -> String
getIdHash (IssueId s)
  | hasPrefix "bd-" s = drop 3 s
  | otherwise = s

-- | Check if an ID is valid
isValidId :: IssueId -> Boolean
isValidId (IssueId s) =
  hasPrefix "bd-" s && length (drop 3 s) >= 4
