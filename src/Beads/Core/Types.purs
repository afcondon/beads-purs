module Beads.Core.Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString, decodeInt, decodeArray, decodeMaybe)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Encoders (encodeString, encodeInt, encodeArray)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | Hash-based issue ID to prevent merge collisions
-- | Format: "bd-xxxx" where xxxx is a hash prefix
newtype IssueId = IssueId String

derive instance Newtype IssueId _
derive instance Eq IssueId
derive instance Ord IssueId
derive newtype instance Show IssueId
derive newtype instance EncodeJson IssueId
derive newtype instance DecodeJson IssueId

-- | Issue status
data Status
  = Open
  | InProgress
  | Blocked
  | Deferred
  | Closed
  | Tombstone  -- soft delete marker

derive instance Eq Status
derive instance Generic Status _
instance Show Status where show = genericShow

instance EncodeJson Status where
  encodeJson = encodeString <<< statusToString

instance DecodeJson Status where
  decodeJson json = do
    s <- decodeString json
    case stringToStatus s of
      Just status -> Right status
      Nothing -> Left $ UnexpectedValue (fromString s)

statusToString :: Status -> String
statusToString = case _ of
  Open -> "open"
  InProgress -> "in_progress"
  Blocked -> "blocked"
  Deferred -> "deferred"
  Closed -> "closed"
  Tombstone -> "tombstone"

stringToStatus :: String -> Maybe Status
stringToStatus = case _ of
  "open" -> Just Open
  "in_progress" -> Just InProgress
  "blocked" -> Just Blocked
  "deferred" -> Just Deferred
  "closed" -> Just Closed
  "tombstone" -> Just Tombstone
  _ -> Nothing

-- | Issue priority (P0 = critical, P4 = backlog)
data Priority = P0 | P1 | P2 | P3 | P4

derive instance Eq Priority
derive instance Ord Priority
derive instance Generic Priority _
instance Show Priority where show = genericShow

instance EncodeJson Priority where
  encodeJson = encodeInt <<< priorityToInt

instance DecodeJson Priority where
  decodeJson json = do
    n <- decodeInt json
    case intToPriority n of
      Just p -> Right p
      Nothing -> Left $ UnexpectedValue json

priorityToInt :: Priority -> Int
priorityToInt = case _ of
  P0 -> 0
  P1 -> 1
  P2 -> 2
  P3 -> 3
  P4 -> 4

intToPriority :: Int -> Maybe Priority
intToPriority = case _ of
  0 -> Just P0
  1 -> Just P1
  2 -> Just P2
  3 -> Just P3
  4 -> Just P4
  _ -> Nothing

-- | Issue type classification
data IssueType
  = Bug
  | Feature
  | Task
  | Epic
  | Chore
  | Message
  | MergeRequest
  | Molecule  -- template instance
  | Gate
  | Agent
  | Role
  | Convoy

derive instance Eq IssueType
derive instance Generic IssueType _
instance Show IssueType where show = genericShow

instance EncodeJson IssueType where
  encodeJson = encodeString <<< issueTypeToString

instance DecodeJson IssueType where
  decodeJson json = do
    s <- decodeString json
    case stringToIssueType s of
      Just t -> Right t
      Nothing -> Left $ UnexpectedValue (fromString s)

issueTypeToString :: IssueType -> String
issueTypeToString = case _ of
  Bug -> "bug"
  Feature -> "feature"
  Task -> "task"
  Epic -> "epic"
  Chore -> "chore"
  Message -> "message"
  MergeRequest -> "merge-request"
  Molecule -> "molecule"
  Gate -> "gate"
  Agent -> "agent"
  Role -> "role"
  Convoy -> "convoy"

stringToIssueType :: String -> Maybe IssueType
stringToIssueType = case _ of
  "bug" -> Just Bug
  "feature" -> Just Feature
  "task" -> Just Task
  "epic" -> Just Epic
  "chore" -> Just Chore
  "message" -> Just Message
  "merge-request" -> Just MergeRequest
  "molecule" -> Just Molecule
  "gate" -> Just Gate
  "agent" -> Just Agent
  "role" -> Just Role
  "convoy" -> Just Convoy
  _ -> Nothing

-- | Dependency type between issues
data DependencyType
  = Blocks         -- X must close before Y starts
  | ParentChild    -- hierarchical (epic/subtask)
  | Related        -- informational link
  | DiscoveredFrom -- Y was found while working on X

derive instance Eq DependencyType
derive instance Generic DependencyType _
instance Show DependencyType where show = genericShow

instance EncodeJson DependencyType where
  encodeJson = encodeString <<< depTypeToString

instance DecodeJson DependencyType where
  decodeJson json = do
    s <- decodeString json
    case stringToDepType s of
      Just t -> Right t
      Nothing -> Left $ UnexpectedValue json

depTypeToString :: DependencyType -> String
depTypeToString = case _ of
  Blocks -> "blocks"
  ParentChild -> "parent-child"
  Related -> "related"
  DiscoveredFrom -> "discovered-from"

stringToDepType :: String -> Maybe DependencyType
stringToDepType = case _ of
  "blocks" -> Just Blocks
  "parent-child" -> Just ParentChild
  "related" -> Just Related
  "discovered-from" -> Just DiscoveredFrom
  _ -> Nothing

-- | A dependency to another issue
-- | Note: issue_id is redundant (it's the parent issue), but included for compatibility
type Dependency =
  { issueId :: IssueId      -- the issue that has this dependency
  , dependsOnId :: IssueId  -- the issue it depends on
  , depType :: DependencyType
  , createdAt :: Maybe String
  , createdBy :: Maybe String
  }

-- | A comment on an issue
type Comment =
  { id :: String
  , author :: Maybe String
  , content :: String
  , createdAt :: String  -- ISO8601 timestamp as string for now
  }

-- | The main Issue record
-- | Note: timestamps are strings (ISO8601) for JSONL compatibility
type Issue =
  { id :: IssueId
  , title :: String
  , description :: Maybe String
  , status :: Status
  , priority :: Int  -- stored as int in JSONL
  , issueType :: Maybe String  -- stored as string, defaults to "task"

  -- Assignment
  , assignee :: Maybe String
  , estimatedMinutes :: Maybe Int

  -- Timestamps (ISO8601 strings)
  , createdAt :: String
  , createdBy :: Maybe String
  , updatedAt :: String
  , closedAt :: Maybe String
  , closeReason :: Maybe String

  -- Relations
  , labels :: Array String
  , dependencies :: Array Dependency
  , comments :: Array Comment

  -- External integration
  , externalRef :: Maybe String

  -- Design fields
  , design :: Maybe String
  , acceptanceCriteria :: Maybe String
  , notes :: Maybe String

  -- Soft delete
  , deletedAt :: Maybe String
  , deletedBy :: Maybe String
  , deleteReason :: Maybe String
  , originalType :: Maybe String
  }
