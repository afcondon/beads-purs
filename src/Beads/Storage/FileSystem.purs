module Beads.Storage.FileSystem where

import Prelude

import Beads.Core.Types (Issue, IssueId)
import Beads.Storage.JSONL (parseJSONL, serializeJSONL)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, try, attempt)
import Effect.Exception (Error, message)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile, writeTextFile, stat)
import Node.Path (FilePath)
import Node.Path as Path

-- | Standard beads directory name
beadsDir :: String
beadsDir = ".beads"

-- | Standard issues file name
issuesFile :: String
issuesFile = "issues.jsonl"

-- | Get the path to the beads directory
getBeadsPath :: FilePath -> FilePath
getBeadsPath projectRoot = Path.concat [projectRoot, beadsDir]

-- | Get the path to the issues file
getIssuesPath :: FilePath -> FilePath
getIssuesPath projectRoot = Path.concat [projectRoot, beadsDir, issuesFile]

-- | Check if beads is initialized in a directory
isInitialized :: FilePath -> Aff Boolean
isInitialized projectRoot = do
  result <- attempt $ stat (getBeadsPath projectRoot)
  pure $ case result of
    Left _ -> false
    Right _ -> true

-- | Initialize beads in a directory
-- | Creates .beads/ directory and empty issues.jsonl
initialize :: FilePath -> Aff (Either Error Unit)
initialize projectRoot = try do
  let beadsPath = getBeadsPath projectRoot
  let issuesPath = getIssuesPath projectRoot
  mkdir beadsPath
  writeTextFile UTF8 issuesPath ""

-- | Read all issues from the JSONL file
readIssues :: FilePath -> Aff (Either String (Array Issue))
readIssues projectRoot = do
  let issuesPath = getIssuesPath projectRoot
  result <- attempt $ readTextFile UTF8 issuesPath
  pure case result of
    Left err -> Left $ "Could not read issues file: " <> message err
    Right content ->
      if content == ""
        then Right []
        else parseJSONL content

-- | Write all issues to the JSONL file
writeIssues :: FilePath -> Array Issue -> Aff (Either Error Unit)
writeIssues projectRoot issues = try do
  let issuesPath = getIssuesPath projectRoot
  let content = if issues == []
        then ""
        else serializeJSONL issues <> "\n"
  writeTextFile UTF8 issuesPath content

-- | Load issues into a Map for efficient lookup
loadIssueMap :: FilePath -> Aff (Either String (Map IssueId Issue))
loadIssueMap projectRoot = do
  result <- readIssues projectRoot
  pure $ map toMap result
  where
  toMap :: Array Issue -> Map IssueId Issue
  toMap issues = Map.fromFoldable $ map (\i -> Tuple i.id i) issues

-- | Save a Map of issues back to the file
saveIssueMap :: FilePath -> Map IssueId Issue -> Aff (Either Error Unit)
saveIssueMap projectRoot issueMap =
  writeIssues projectRoot (Array.fromFoldable $ Map.values issueMap)
