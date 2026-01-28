module Beads.App where

import Prelude

import Beads.Core.Commands as Cmd
import Beads.Core.Id (generateId)
import Beads.Core.Queries as Q
import Beads.Core.Store (Store)
import Beads.Core.Store as Store
import Beads.Core.Types (Issue, IssueId)
import Beads.Storage.JSONL (parseJSONL, serializeJSONL)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, throwError, error)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, readTextFile, stat, writeTextFile)
import Node.Path (FilePath)
import Node.Process (cwd)

-- | Check if a file or directory exists
exists :: FilePath -> Aff Boolean
exists path = do
  result <- attempt $ stat path
  pure $ case result of
    Left _ -> false
    Right _ -> true

-- | Configuration for a beads repo
type Config =
  { root :: FilePath      -- Project root (where .beads/ lives)
  , issuesFile :: FilePath -- Full path to issues.jsonl
  , autoCommit :: Boolean  -- Git auto-commit on write
  }

-- | Find the .beads directory starting from current directory
-- | For now, just uses cwd/.beads
findBeadsDir :: Aff Config
findBeadsDir = do
  root <- liftEffect cwd
  let beadsDir = root <> "/.beads"
  let issuesFile = beadsDir <> "/issues.jsonl"
  pure { root, issuesFile, autoCommit: true }

-- | Initialize a new beads repo
initRepo :: Aff { created :: Boolean, path :: FilePath }
initRepo = do
  root <- liftEffect cwd
  let beadsDir = root <> "/.beads"
  let issuesFile = beadsDir <> "/issues.jsonl"

  dirExists <- exists beadsDir
  if dirExists
    then pure { created: false, path: beadsDir }
    else do
      mkdir beadsDir
      writeTextFile UTF8 issuesFile ""
      -- Git commit the init
      _ <- gitCommit root "Initialize beads issue tracker"
      pure { created: true, path: beadsDir }

-- | Load the store from disk
loadStore :: Config -> Aff Store
loadStore config = do
  fileExists <- exists config.issuesFile
  if not fileExists
    then pure Store.empty
    else do
      content <- readTextFile UTF8 config.issuesFile
      case parseJSONL content of
        Left err -> throwError $ error $ "Failed to parse issues: " <> err
        Right issues -> pure $ Store.fromArray issues

-- | Save the store to disk and optionally git commit
saveStore :: Config -> String -> Store -> Aff Unit
saveStore config commitMsg store = do
  let content = serializeJSONL (Store.toArray store)
  writeTextFile UTF8 config.issuesFile content
  when config.autoCommit do
    _ <- gitCommit config.root commitMsg
    pure unit

-- | Get current timestamp in ISO 8601 format
getTimestamp :: Effect String
getTimestamp = do
  instant <- now
  let dt = toDateTime instant
  case formatDateTime "YYYY-MM-DDTHH:mm:ssZ" dt of
    Left _ -> pure "1970-01-01T00:00:00Z"  -- fallback
    Right s -> pure s

-- | Run a git command in the repo
gitCommit :: FilePath -> String -> Aff Boolean
gitCommit repoPath message = do
  result <- attempt $ liftEffect do
    -- Stage the issues file
    _ <- CP.execSync' ("git add .beads/issues.jsonl") (_ { cwd = Just repoPath })
    -- Commit
    _ <- CP.execSync' ("git commit -m \"" <> escapeForShell message <> "\" --allow-empty")
           (_ { cwd = Just repoPath })
    pure unit
  case result of
    Left _ -> pure false
    Right _ -> pure true
  where
  escapeForShell s = s -- TODO: proper escaping

-- | Create a new issue
createIssue :: String -> Maybe String -> Int -> Aff { id :: IssueId, store :: Store }
createIssue title description priority = do
  config <- findBeadsDir
  store <- loadStore config
  id <- liftEffect generateId
  timestamp <- liftEffect getTimestamp

  let newIssue =
        { title
        , description
        , priority
        , issueType: Just "task"
        , labels: []
        , assignee: Nothing
        }

  case Cmd.create id timestamp newIssue store of
    Left err -> throwError $ error $ show err
    Right store' -> do
      saveStore config ("beads: create " <> show id <> " - " <> title) store'
      pure { id, store: store' }

-- | Close an issue
closeIssue :: IssueId -> String -> Aff Store
closeIssue id reason = do
  config <- findBeadsDir
  store <- loadStore config
  timestamp <- liftEffect getTimestamp

  case Cmd.close id timestamp reason store of
    Left err -> throwError $ error $ show err
    Right store' -> do
      saveStore config ("beads: close " <> show id <> " - " <> reason) store'
      pure store'

-- | Get ready issues sorted by priority
getReady :: Aff (Array Issue)
getReady = do
  config <- findBeadsDir
  store <- loadStore config
  pure $ Q.readyByPriority store

-- | Get all open issues
getOpen :: Aff (Array Issue)
getOpen = do
  config <- findBeadsDir
  store <- loadStore config
  pure $ Q.openIssues store

-- | Get statistics
getStats :: Aff Q.Stats
getStats = do
  config <- findBeadsDir
  store <- loadStore config
  pure $ Q.stats store
