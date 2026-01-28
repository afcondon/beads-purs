module Beads.Storage.JSONL where

import Prelude

import Beads.Core.Issue (decodeIssue, encodeIssue)
import Beads.Core.Types (Issue)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, intercalate)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.String (Pattern(..), null, split, trim)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

-- | Parse a JSONL string into an array of Issues
-- | Each line is a separate JSON object
parseJSONL :: String -> Either String (Array Issue)
parseJSONL content =
  content
    # split (Pattern "\n")
    # filter (not <<< null <<< trim)
    # traverse parseLine
  where
  parseLine :: String -> Either String Issue
  parseLine line = do
    json <- case jsonParser line of
      Left err -> Left $ "JSON parse error: " <> err
      Right j -> Right j
    case decodeIssue json of
      Left err -> Left $ "Decode error: " <> show err
      Right issue -> Right issue

-- | Parse JSONL, ignoring any lines that fail to parse
-- | Useful for robustness when some lines might be malformed
parseJSONLLenient :: String -> Array Issue
parseJSONLLenient content =
  content
    # split (Pattern "\n")
    # filter (not <<< null <<< trim)
    # map parseLine
    # filter isJust
    # map (unsafePartial fromJust)
  where
  parseLine :: String -> Maybe Issue
  parseLine line = do
    json <- hush $ jsonParser line
    hush $ decodeIssue json

-- | Serialize an array of Issues to JSONL format
serializeJSONL :: Array Issue -> String
serializeJSONL issues =
  issues
    # map (stringify <<< encodeIssue)
    # intercalate "\n"
