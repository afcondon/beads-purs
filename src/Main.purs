module Main where

import Prelude

import Beads.Core.Graph (buildGraph, readyByPriority, stats)
import Beads.Core.Types (Issue, IssueId(..))
import Beads.Storage.JSONL (parseJSONL)
import Data.Array (length, take)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "=== Beads PureScript Test ==="
  liftEffect $ log ""

  let issuesPath = "/Users/afc/work/afc-work/GitHub/beads/.beads/issues.jsonl.new"

  content <- readTextFile UTF8 issuesPath

  case parseJSONL content of
    Left err -> liftEffect $ log $ "Parse error: " <> err
    Right issues -> do
      liftEffect $ log $ "Loaded " <> show (length issues) <> " issues"
      liftEffect $ log ""

      -- Build the graph
      let graph = buildGraph issues

      -- Get stats
      let s = stats graph
      liftEffect $ log "=== Graph Statistics ==="
      liftEffect $ log $ "  Total:   " <> show s.total
      liftEffect $ log $ "  Open:    " <> show s.open
      liftEffect $ log $ "  Closed:  " <> show s.closed
      liftEffect $ log $ "  Ready:   " <> show s.ready
      liftEffect $ log $ "  Blocked: " <> show s.blocked
      liftEffect $ log ""

      -- Get ready issues
      let readyIssues = readyByPriority graph
      liftEffect $ log "=== Ready Issues (top 10 by priority) ==="

      -- Show top 10
      let top10 = take 10 readyIssues
      for_ top10 \issue -> do
        liftEffect $ log $ formatIssue issue

formatIssue :: Issue -> String
formatIssue issue =
  "  [P" <> show issue.priority <> "] " <> showId issue.id <> ": " <> truncate 50 issue.title
  where
  showId (IssueId s) = s

  truncate :: Int -> String -> String
  truncate n s =
    if String.length s <= n
      then s
      else String.take n s <> "..."
