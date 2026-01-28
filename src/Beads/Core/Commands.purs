module Beads.Core.Commands where

import Prelude

import Beads.Core.Store (Store)
import Beads.Core.Store as Store
import Beads.Core.Types (Issue, IssueId, Status(..), Dependency, DependencyType)
import Data.Array (filter, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Errors that can occur during commands
data CommandError
  = IssueNotFound IssueId
  | IssueAlreadyExists IssueId
  | CannotCloseAlreadyClosed IssueId
  | SelfDependency IssueId

instance Show CommandError where
  show = case _ of
    IssueNotFound id -> "Issue not found: " <> show id
    IssueAlreadyExists id -> "Issue already exists: " <> show id
    CannotCloseAlreadyClosed id -> "Issue already closed: " <> show id
    SelfDependency id -> "Cannot create self-dependency: " <> show id

-- | Result of a command
type CommandResult = Either CommandError Store

-- | Input for creating a new issue (no ID or timestamps - those come from effects)
type NewIssue =
  { title :: String
  , description :: Maybe String
  , priority :: Int
  , issueType :: Maybe String
  , labels :: Array String
  , assignee :: Maybe String
  }

-- | Create a new issue
-- | ID and timestamp are provided by the effectful layer
create :: IssueId -> String -> NewIssue -> Store -> CommandResult
create id timestamp input store =
  if Store.member id store
    then Left $ IssueAlreadyExists id
    else Right $ Store.insert (mkIssue id timestamp input) store

-- | Construct a full Issue from NewIssue + generated fields
mkIssue :: IssueId -> String -> NewIssue -> Issue
mkIssue id timestamp input =
  { id
  , title: input.title
  , description: input.description
  , status: Open
  , priority: input.priority
  , issueType: input.issueType
  , assignee: input.assignee
  , estimatedMinutes: Nothing
  , createdAt: timestamp
  , createdBy: Nothing
  , updatedAt: timestamp
  , closedAt: Nothing
  , closeReason: Nothing
  , labels: input.labels
  , dependencies: []
  , comments: []
  , externalRef: Nothing
  , design: Nothing
  , acceptanceCriteria: Nothing
  , notes: Nothing
  , deletedAt: Nothing
  , deletedBy: Nothing
  , deleteReason: Nothing
  , originalType: Nothing
  }

-- | Update an existing issue with a transformation function
-- | The function also receives the current timestamp for updatedAt
modify :: IssueId -> String -> (Issue -> Issue) -> Store -> CommandResult
modify id timestamp f store =
  case Store.lookup id store of
    Nothing -> Left $ IssueNotFound id
    Just issue ->
      let updated = f issue
          withTimestamp = updated { updatedAt = timestamp }
      in Right $ Store.insert withTimestamp store

-- | Close an issue
close :: IssueId -> String -> String -> Store -> CommandResult
close id timestamp reason store =
  case Store.lookup id store of
    Nothing -> Left $ IssueNotFound id
    Just issue ->
      if isClosed issue.status
        then Left $ CannotCloseAlreadyClosed id
        else Right $ Store.insert (closeIssue timestamp reason issue) store
  where
  isClosed Closed = true
  isClosed Tombstone = true
  isClosed _ = false

  closeIssue ts r i = i
    { status = Closed
    , closedAt = Just ts
    , closeReason = Just r
    , updatedAt = ts
    }

-- | Reopen a closed issue
reopen :: IssueId -> String -> Store -> CommandResult
reopen id timestamp store =
  case Store.lookup id store of
    Nothing -> Left $ IssueNotFound id
    Just issue -> Right $ Store.insert (reopenIssue timestamp issue) store
  where
  reopenIssue ts i = i
    { status = Open
    , closedAt = Nothing
    , closeReason = Nothing
    , updatedAt = ts
    }

-- | Set issue status
setStatus :: IssueId -> String -> Status -> Store -> CommandResult
setStatus id timestamp status store =
  modify id timestamp (_ { status = status }) store

-- | Set issue priority
setPriority :: IssueId -> String -> Int -> Store -> CommandResult
setPriority id timestamp priority store =
  modify id timestamp (_ { priority = priority }) store

-- | Set issue title
setTitle :: IssueId -> String -> String -> Store -> CommandResult
setTitle id timestamp title store =
  modify id timestamp (_ { title = title }) store

-- | Add a dependency between two issues
-- | fromId depends on toId (toId blocks fromId)
addDep :: IssueId -> IssueId -> String -> DependencyType -> Store -> CommandResult
addDep fromId toId timestamp depType store
  | fromId == toId = Left $ SelfDependency fromId
  | not (Store.member fromId store) = Left $ IssueNotFound fromId
  | not (Store.member toId store) = Left $ IssueNotFound toId
  | otherwise =
      let dep = mkDependency fromId toId timestamp depType
      in modify fromId timestamp (addDepToIssue dep) store

mkDependency :: IssueId -> IssueId -> String -> DependencyType -> Dependency
mkDependency fromId toId timestamp depType =
  { issueId: fromId
  , dependsOnId: toId
  , depType
  , createdAt: Just timestamp
  , createdBy: Nothing
  }

addDepToIssue :: Dependency -> Issue -> Issue
addDepToIssue dep issue =
  -- Don't add if already exists
  if hasDep dep.dependsOnId issue.dependencies
    then issue
    else issue { dependencies = snoc issue.dependencies dep }
  where
  hasDep targetId deps =
    deps # filter (\d -> d.dependsOnId == targetId) # (_ /= [])

-- | Remove a dependency
removeDep :: IssueId -> IssueId -> String -> Store -> CommandResult
removeDep fromId toId timestamp store =
  modify fromId timestamp (removeDepFromIssue toId) store

removeDepFromIssue :: IssueId -> Issue -> Issue
removeDepFromIssue toId issue =
  issue { dependencies = filter (\d -> d.dependsOnId /= toId) issue.dependencies }

-- | Add a label to an issue
addLabel :: IssueId -> String -> String -> Store -> CommandResult
addLabel id timestamp label store =
  modify id timestamp addLabelToIssue store
  where
  addLabelToIssue issue =
    if label `elem` issue.labels
      then issue
      else issue { labels = snoc issue.labels label }
  elem x xs = filter (_ == x) xs /= []

-- | Remove a label from an issue
removeLabel :: IssueId -> String -> String -> Store -> CommandResult
removeLabel id timestamp label store =
  modify id timestamp (\i -> i { labels = filter (_ /= label) i.labels }) store

-- | Soft delete (tombstone) an issue
softDelete :: IssueId -> String -> String -> String -> Store -> CommandResult
softDelete id timestamp deletedBy reason store =
  modify id timestamp tombstone store
  where
  tombstone issue = issue
    { status = Tombstone
    , deletedAt = Just timestamp
    , deletedBy = Just deletedBy
    , deleteReason = Just reason
    , originalType = issue.issueType
    }
