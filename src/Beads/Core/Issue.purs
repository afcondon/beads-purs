module Beads.Core.Issue where

import Prelude

import Beads.Core.Types (Issue, IssueId(..), Status(..), Dependency, Comment)
import Data.Argonaut.Core (Json, jsonEmptyObject, fromObject, fromArray, fromString, toObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional, getFieldOptional')
import Data.Either (note)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Data.Array (filter, (:))

-- | Decode a Dependency from JSON
decodeDependency :: Json -> Either JsonDecodeError Dependency
decodeDependency json = do
  obj <- note (TypeMismatch "Expected object") (toObject json)
  issueId <- getField obj "issue_id"
  dependsOnId <- getField obj "depends_on_id"
  depType <- getField obj "type"
  createdAt <- getFieldOptional obj "created_at"
  createdBy <- getFieldOptional obj "created_by"
  pure { issueId, dependsOnId, depType, createdAt, createdBy }

-- | Encode a Dependency to JSON
encodeDependency :: Dependency -> Json
encodeDependency dep =
  fromObject $ FO.fromFoldable $ catMaybes
    [ Just $ Tuple "issue_id" (encodeJson dep.issueId)
    , Just $ Tuple "depends_on_id" (encodeJson dep.dependsOnId)
    , Just $ Tuple "type" (encodeJson dep.depType)
    , map (\t -> Tuple "created_at" (encodeJson t)) dep.createdAt
    , map (\t -> Tuple "created_by" (encodeJson t)) dep.createdBy
    ]

-- | Decode a Comment from JSON
decodeComment :: Json -> Either JsonDecodeError Comment
decodeComment json = do
  obj <- note (TypeMismatch "Expected object") (toObject json)
  id <- getField obj "id"
  author <- getFieldOptional obj "author"
  content <- getField obj "content"
  createdAt <- getField obj "created_at"
  pure { id, author, content, createdAt }

-- | Encode a Comment to JSON
encodeComment :: Comment -> Json
encodeComment c =
  fromObject $ FO.fromFoldable $ catMaybes
    [ Just $ Tuple "id" (encodeJson c.id)
    , map (\a -> Tuple "author" (encodeJson a)) c.author
    , Just $ Tuple "content" (encodeJson c.content)
    , Just $ Tuple "created_at" (encodeJson c.createdAt)
    ]

-- | Decode an Issue from JSON (JSONL format)
decodeIssue :: Json -> Either JsonDecodeError Issue
decodeIssue json = do
  obj <- note (TypeMismatch "Expected object") (toObject json)
  id <- getField obj "id"
  title <- getField obj "title"
  description <- getFieldOptional obj "description"
  status <- fromMaybe Open <$> getFieldOptional obj "status"
  priority <- fromMaybe 2 <$> getFieldOptional obj "priority"
  issueType <- getFieldOptional obj "issue_type"

  assignee <- getFieldOptional obj "assignee"
  estimatedMinutes <- getFieldOptional obj "estimated_minutes"

  createdAt <- getField obj "created_at"
  createdBy <- getFieldOptional obj "created_by"
  updatedAt <- getField obj "updated_at"
  closedAt <- getFieldOptional obj "closed_at"
  closeReason <- getFieldOptional obj "close_reason"

  labels <- fromMaybe [] <$> getFieldOptional obj "labels"

  -- Dependencies need special handling
  depsJson <- getFieldOptional' obj "dependencies"
  dependencies <- case depsJson of
    Nothing -> pure []
    Just arr -> traverse decodeDependency arr

  -- Comments need special handling
  commentsJson <- getFieldOptional' obj "comments"
  comments <- case commentsJson of
    Nothing -> pure []
    Just arr -> traverse decodeComment arr

  externalRef <- getFieldOptional obj "external_ref"

  design <- getFieldOptional obj "design"
  acceptanceCriteria <- getFieldOptional obj "acceptance_criteria"
  notes <- getFieldOptional obj "notes"

  deletedAt <- getFieldOptional obj "deleted_at"
  deletedBy <- getFieldOptional obj "deleted_by"
  deleteReason <- getFieldOptional obj "delete_reason"
  originalType <- getFieldOptional obj "original_type"

  pure
    { id
    , title
    , description
    , status
    , priority
    , issueType
    , assignee
    , estimatedMinutes
    , createdAt
    , createdBy
    , updatedAt
    , closedAt
    , closeReason
    , labels
    , dependencies
    , comments
    , externalRef
    , design
    , acceptanceCriteria
    , notes
    , deletedAt
    , deletedBy
    , deleteReason
    , originalType
    }

-- | Encode an Issue to JSON (JSONL format)
-- | Only includes non-empty optional fields (omitempty behavior)
encodeIssue :: Issue -> Json
encodeIssue i =
  fromObject $ FO.fromFoldable $ catMaybes
    [ required "id" i.id
    , required "title" i.title
    , optional "description" i.description
    , required "status" i.status
    , required "priority" i.priority
    , optional "issue_type" i.issueType
    , optional "assignee" i.assignee
    , optional "estimated_minutes" i.estimatedMinutes
    , required "created_at" i.createdAt
    , optional "created_by" i.createdBy
    , required "updated_at" i.updatedAt
    , optional "closed_at" i.closedAt
    , optional "close_reason" i.closeReason
    , nonEmpty "labels" i.labels
    , nonEmptyWith "dependencies" encodeDependency i.dependencies
    , nonEmptyWith "comments" encodeComment i.comments
    , optional "external_ref" i.externalRef
    , optional "design" i.design
    , optional "acceptance_criteria" i.acceptanceCriteria
    , optional "notes" i.notes
    , optional "deleted_at" i.deletedAt
    , optional "deleted_by" i.deletedBy
    , optional "delete_reason" i.deleteReason
    , optional "original_type" i.originalType
    ]
  where
  required :: forall a. EncodeJson a => String -> a -> Maybe (Tuple String Json)
  required key val = Just $ Tuple key (encodeJson val)

  optional :: forall a. EncodeJson a => String -> Maybe a -> Maybe (Tuple String Json)
  optional key = map (\v -> Tuple key (encodeJson v))

  nonEmpty :: forall a. EncodeJson a => String -> Array a -> Maybe (Tuple String Json)
  nonEmpty _ [] = Nothing
  nonEmpty key arr = Just $ Tuple key (encodeJson arr)

  nonEmptyWith :: forall a. String -> (a -> Json) -> Array a -> Maybe (Tuple String Json)
  nonEmptyWith _ _ [] = Nothing
  nonEmptyWith key f arr = Just $ Tuple key (fromArray (map f arr))

-- | Filter out Nothing values from an array
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes arr = do
  mx <- arr
  case mx of
    Nothing -> []
    Just x -> [x]

-- | Create a new issue with defaults
mkIssue :: IssueId -> String -> String -> Issue
mkIssue id title timestamp =
  { id
  , title
  , description: Nothing
  , status: Open
  , priority: 2  -- P2
  , issueType: Just "task"
  , assignee: Nothing
  , estimatedMinutes: Nothing
  , createdAt: timestamp
  , createdBy: Nothing
  , updatedAt: timestamp
  , closedAt: Nothing
  , closeReason: Nothing
  , labels: []
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

