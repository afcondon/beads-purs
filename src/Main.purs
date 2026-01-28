module Main where

import Prelude

import Beads.App as App
import Beads.Core.Types (Issue, IssueId(..))
import Data.Array (drop, head, intercalate, take, (!!))
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, message, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect argv
  -- args: [node, script, command, ...args]
  let command = args !! 2
  let cmdArgs = drop 3 args

  case command of
    Just "init" -> cmdInit
    Just "create" -> cmdCreate cmdArgs
    Just "ready" -> cmdReady cmdArgs
    Just "close" -> cmdClose cmdArgs
    Just "list" -> cmdList cmdArgs
    Just "show" -> cmdShow cmdArgs
    Just "dep" -> cmdDep cmdArgs
    Just "edit" -> cmdEdit cmdArgs
    Just "stats" -> cmdStats
    Just "help" -> cmdHelp
    Just cmd -> do
      liftEffect $ log $ "Unknown command: " <> cmd
      cmdHelp
    Nothing -> cmdHelp

-- | Check if beads is initialized, run action if so, otherwise warn
requireInit :: Aff Unit -> Aff Unit
requireInit action = do
  initialized <- App.isInitialized
  if initialized
    then action
    else liftEffect $ log "Not a beads repo. Run 'bd init' first."

-- | bd init - initialize a beads repo
cmdInit :: Aff Unit
cmdInit = do
  result <- try $ App.initRepo
  case result of
    Left err -> liftEffect $ log $ "Error: " <> message err
    Right { created, path } ->
      if created
        then liftEffect $ log $ "Initialized beads repo at " <> path
        else liftEffect $ log $ "Beads repo already exists at " <> path

-- | bd create "title" [-p priority] [-d description]
cmdCreate :: Array String -> Aff Unit
cmdCreate args = requireInit do
  case head args of
    Nothing -> liftEffect $ log "Usage: bd create \"title\" [-p priority]"
    Just title -> do
      let priority = fromMaybe 2 $ parseFlag "-p" args >>= parseInt
      let description = parseFlag "-d" args
      result <- try $ App.createIssue title description priority
      case result of
        Left err -> liftEffect $ log $ "Error: " <> message err
        Right { id } -> do
          liftEffect $ log $ "Created: " <> showId id
          liftEffect $ log $ "  Title: " <> title
          liftEffect $ log $ "  Priority: P" <> show priority

-- | bd ready [-n limit]
cmdReady :: Array String -> Aff Unit
cmdReady args = do
  requireInit do
    let limit = fromMaybe 10 $ parseFlag "-n" args >>= parseInt
    result <- try $ App.getReady
    case result of
      Left err -> liftEffect $ log $ "Error: " <> message err
      Right issues -> do
        liftEffect $ log "Ready issues (by priority):"
        liftEffect $ log ""
        for_ (take limit issues) \issue -> do
          liftEffect $ log $ formatIssue issue

-- | bd close <id> "reason"
cmdClose :: Array String -> Aff Unit
cmdClose args = requireInit do
  case args !! 0, args !! 1 of
    Just idStr, Just reason -> do
      let id = IssueId idStr
      result <- try $ App.closeIssue id reason
      case result of
        Left err -> liftEffect $ log $ "Error: " <> message err
        Right _ -> liftEffect $ log $ "Closed: " <> idStr
    _, _ -> liftEffect $ log "Usage: bd close <id> \"reason\""

-- | bd list [-n limit] [-s search]
cmdList :: Array String -> Aff Unit
cmdList args = requireInit do
  let limit = fromMaybe 20 $ parseFlag "-n" args >>= parseInt
  let search = parseFlag "-s" args
  result <- try $ case search of
    Just q -> App.searchOpen q
    Nothing -> App.getOpen
  case result of
    Left err -> liftEffect $ log $ "Error: " <> message err
    Right issues -> do
      case search of
        Just q -> liftEffect $ log $ "Open issues matching \"" <> q <> "\":"
        Nothing -> liftEffect $ log "Open issues:"
      liftEffect $ log ""
      for_ (take limit issues) \issue -> do
        liftEffect $ log $ formatIssue issue

-- | bd show <id>
cmdShow :: Array String -> Aff Unit
cmdShow args = requireInit do
  case head args of
    Nothing -> liftEffect $ log "Usage: bd show <id>"
    Just idStr -> do
      let id = IssueId idStr
      result <- try $ App.getIssue id
      case result of
        Left err -> liftEffect $ log $ "Error: " <> message err
        Right Nothing -> liftEffect $ log $ "Issue not found: " <> idStr
        Right (Just issue) -> do
          liftEffect $ log $ "Issue: " <> idStr
          liftEffect $ log $ "  Title:    " <> issue.title
          liftEffect $ log $ "  Status:   " <> show issue.status
          liftEffect $ log $ "  Priority: P" <> show issue.priority
          case issue.description of
            Just d -> liftEffect $ log $ "  Desc:     " <> d
            Nothing -> pure unit
          case issue.issueType of
            Just t -> liftEffect $ log $ "  Type:     " <> t
            Nothing -> pure unit
          when (issue.labels /= []) do
            liftEffect $ log $ "  Labels:   " <> showLabels issue.labels
          when (issue.dependencies /= []) do
            liftEffect $ log $ "  Blocked by:"
            for_ issue.dependencies \dep -> do
              liftEffect $ log $ "    - " <> showId dep.dependsOnId
          liftEffect $ log $ "  Created:  " <> issue.createdAt
          liftEffect $ log $ "  Updated:  " <> issue.updatedAt
          case issue.closedAt of
            Just t -> liftEffect $ log $ "  Closed:   " <> t
            Nothing -> pure unit
          case issue.closeReason of
            Just r -> liftEffect $ log $ "  Reason:   " <> r
            Nothing -> pure unit

-- | bd dep add|rm <from> <to>
cmdDep :: Array String -> Aff Unit
cmdDep args = requireInit do
  case args !! 0, args !! 1, args !! 2 of
    Just "add", Just fromStr, Just toStr -> do
      let fromId = IssueId fromStr
      let toId = IssueId toStr
      result <- try $ App.addDependency fromId toId
      case result of
        Left err -> liftEffect $ log $ "Error: " <> message err
        Right _ -> liftEffect $ log $ fromStr <> " is now blocked by " <> toStr
    Just "rm", Just fromStr, Just toStr -> do
      let fromId = IssueId fromStr
      let toId = IssueId toStr
      result <- try $ App.removeDependency fromId toId
      case result of
        Left err -> liftEffect $ log $ "Error: " <> message err
        Right _ -> liftEffect $ log $ fromStr <> " is no longer blocked by " <> toStr
    _, _, _ -> do
      liftEffect $ log "Usage:"
      liftEffect $ log "  bd dep add <from> <to>  - from is blocked by to"
      liftEffect $ log "  bd dep rm <from> <to>   - remove dependency"

-- | bd edit <id> [-t title] [-p priority]
cmdEdit :: Array String -> Aff Unit
cmdEdit args = requireInit do
  case head args of
    Nothing -> liftEffect $ log "Usage: bd edit <id> [-t title] [-p priority]"
    Just idStr -> do
      let id = IssueId idStr
      let newTitle = parseFlag "-t" args
      let newPriority = parseFlag "-p" args >>= parseInt

      case newTitle, newPriority of
        Nothing, Nothing -> liftEffect $ log "Nothing to edit. Use -t or -p."
        Just t, _ -> do
          result <- try $ App.setTitle id t
          case result of
            Left err -> liftEffect $ log $ "Error: " <> message err
            Right _ -> liftEffect $ log $ "Updated title: " <> t
        Nothing, Just p -> do
          result <- try $ App.setPriority id p
          case result of
            Left err -> liftEffect $ log $ "Error: " <> message err
            Right _ -> liftEffect $ log $ "Updated priority: P" <> show p

-- | bd stats
cmdStats :: Aff Unit
cmdStats = requireInit do
  result <- try $ App.getStats
  case result of
    Left err -> liftEffect $ log $ "Error: " <> message err
    Right s -> do
      liftEffect $ log "Statistics:"
      liftEffect $ log $ "  Total:   " <> show s.total
      liftEffect $ log $ "  Open:    " <> show s.open
      liftEffect $ log $ "  Closed:  " <> show s.closed
      liftEffect $ log $ "  Ready:   " <> show s.ready
      liftEffect $ log $ "  Blocked: " <> show s.blocked

-- | bd help
cmdHelp :: Aff Unit
cmdHelp = liftEffect do
  log "beads-purs - A PureScript issue tracker"
  log ""
  log "Commands:"
  log "  init              Initialize a beads repo in current directory"
  log "  create \"title\"    Create a new issue"
  log "    -p <priority>   Set priority (0-4, default 2)"
  log "    -d \"desc\"       Set description"
  log "  ready             Show issues ready to work on"
  log "    -n <limit>      Limit results (default 10)"
  log "  list              Show all open issues"
  log "    -n <limit>      Limit results (default 20)"
  log "    -s <query>      Search by title"
  log "  show <id>         Show issue details"
  log "  close <id> \"why\"  Close an issue with reason"
  log "  dep add <a> <b>   Make a blocked by b"
  log "  dep rm <a> <b>    Remove dependency"
  log "  edit <id>         Edit an issue"
  log "    -t \"title\"      Set new title"
  log "    -p <priority>   Set new priority"
  log "  stats             Show statistics"
  log "  help              Show this help"

-- | Parse a flag value like "-p 2" from args
parseFlag :: String -> Array String -> Maybe String
parseFlag flag args = go 0
  where
  go i = case args !! i of
    Just f | f == flag -> args !! (i + 1)
    Just _ -> go (i + 1)
    Nothing -> Nothing

-- | Parse an integer
parseInt :: String -> Maybe Int
parseInt s = case String.toCodePointArray s of
  _ -> Just (unsafeParseInt s)  -- TODO: proper parsing

foreign import unsafeParseInt :: String -> Int

-- | Format an issue for display
formatIssue :: Issue -> String
formatIssue issue =
  "[P" <> show issue.priority <> "] " <> showId issue.id <> ": " <> truncate 60 issue.title

showId :: IssueId -> String
showId (IssueId s) = s

truncate :: Int -> String -> String
truncate n s =
  if String.length s <= n
    then s
    else String.take n s <> "..."

showLabels :: Array String -> String
showLabels = intercalate ", "
