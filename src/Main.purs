module Main where

import Prelude

import Beads.Core.Commands as Cmd
import Beads.Core.Id (generateId)
import Beads.Core.Queries as Q
import Beads.Core.Store as Store
import Beads.Core.Types (Issue, IssueId(..))
import Beads.Storage.JSONL (parseJSONL)
import Data.Array (take)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "=== Beads PureScript (New Architecture) ==="
  liftEffect $ log ""

  -- Load issues from the real beads repo
  let issuesPath = "/Users/afc/work/afc-work/GitHub/beads/.beads/issues.jsonl.new"
  content <- readTextFile UTF8 issuesPath

  case parseJSONL content of
    Left err -> liftEffect $ log $ "Parse error: " <> err
    Right issues -> do
      -- Build the store
      let store = Store.fromArray issues

      liftEffect $ log $ "Loaded " <> show (Store.size store) <> " issues into Store"
      liftEffect $ log ""

      -- Run queries
      let s = Q.stats store
      liftEffect $ log "=== Statistics ==="
      liftEffect $ log $ "  Total:   " <> show s.total
      liftEffect $ log $ "  Open:    " <> show s.open
      liftEffect $ log $ "  Closed:  " <> show s.closed
      liftEffect $ log $ "  Ready:   " <> show s.ready
      liftEffect $ log $ "  Blocked: " <> show s.blocked
      liftEffect $ log ""

      -- Show top 5 ready issues
      let readyIssues = Q.readyByPriority store
      liftEffect $ log "=== Ready Issues (top 5) ==="
      for_ (take 5 readyIssues) \issue -> do
        liftEffect $ log $ formatIssue issue
      liftEffect $ log ""

      -- Demonstrate pure commands
      liftEffect $ log "=== Command Demo ==="

      -- Simulate creating a new issue
      newId <- liftEffect generateId
      let timestamp = "2025-01-28T12:00:00Z"
      let newIssue =
            { title: "Port beads to PureScript"
            , description: Just "A cleaner, type-safe implementation"
            , priority: 1
            , issueType: Just "feature"
            , labels: ["purescript", "port"]
            , assignee: Nothing
            }

      case Cmd.create newId timestamp newIssue store of
        Left err -> liftEffect $ log $ "Create failed: " <> show err
        Right store' -> do
          liftEffect $ log $ "Created issue: " <> show newId
          liftEffect $ log $ "  Store size: " <> show (Store.size store) <> " -> " <> show (Store.size store')

          -- Now close it
          case Cmd.close newId timestamp "Completed the port!" store' of
            Left err -> liftEffect $ log $ "Close failed: " <> show err
            Right store'' -> do
              liftEffect $ log $ "Closed issue: " <> show newId

              -- Check stats changed
              let s' = Q.stats store''
              liftEffect $ log $ "  Ready count: " <> show s.ready <> " -> " <> show s'.ready
              liftEffect $ log $ "  Closed count: " <> show s.closed <> " -> " <> show s'.closed

      liftEffect $ log ""
      liftEffect $ log "=== Architecture Demo Complete ==="
      liftEffect $ log "Pure commands, immutable store, effects only at edges"

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
