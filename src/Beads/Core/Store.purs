module Beads.Core.Store where

import Prelude

import Beads.Core.Types (Issue, IssueId)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

-- | The immutable issue store
-- | This is the entire state of the issue tracker
newtype Store = Store (Map IssueId Issue)

derive instance Newtype Store _

-- | Empty store
empty :: Store
empty = Store Map.empty

-- | Build a store from an array of issues
fromArray :: Array Issue -> Store
fromArray issues = Store $ Map.fromFoldable $ map (\i -> Tuple i.id i) issues

-- | Get all issues as an array
toArray :: Store -> Array Issue
toArray (Store m) = Array.fromFoldable $ Map.values m

-- | Look up an issue by ID
lookup :: IssueId -> Store -> Maybe Issue
lookup id (Store m) = Map.lookup id m

-- | Check if an issue exists
member :: IssueId -> Store -> Boolean
member id (Store m) = Map.member id m

-- | Insert or replace an issue
insert :: Issue -> Store -> Store
insert issue (Store m) = Store $ Map.insert issue.id issue m

-- | Update an issue if it exists
-- | Returns the store unchanged if the issue doesn't exist
update :: IssueId -> (Issue -> Issue) -> Store -> Store
update id f (Store m) = Store $ Map.update (pure <<< f) id m

-- | Delete an issue
delete :: IssueId -> Store -> Store
delete id (Store m) = Store $ Map.delete id m

-- | Number of issues in the store
size :: Store -> Int
size (Store m) = Map.size m

-- | Filter issues
filter :: (Issue -> Boolean) -> Store -> Array Issue
filter p (Store m) = Array.filter p $ Array.fromFoldable $ Map.values m

-- | Get the underlying map (for queries that need it)
unwrap :: Store -> Map IssueId Issue
unwrap (Store m) = m
