module Beads.Core.Graph where

import Prelude

import Beads.Core.Types (Issue, IssueId, Status(..), Dependency, DependencyType(..))
import Data.Array (filter, sortBy, length)
import Data.Array as Array
import Data.Foldable (all, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A graph of issues indexed by ID
type IssueGraph = Map IssueId Issue

-- | Build an issue graph from an array of issues
buildGraph :: Array Issue -> IssueGraph
buildGraph issues = Map.fromFoldable $ map (\i -> Tuple i.id i) issues

-- | Check if a status counts as "open" (can be worked on or blocks others)
isOpenStatus :: Status -> Boolean
isOpenStatus = case _ of
  Open -> true
  InProgress -> true
  Blocked -> true  -- still counts as not-done
  _ -> false

-- | Check if an issue is open
isOpen :: Issue -> Boolean
isOpen issue = isOpenStatus issue.status

-- | Check if an issue is closed (done, won't block others)
isClosed :: Issue -> Boolean
isClosed = not <<< isOpen

-- | Get all issues that this issue depends on (blocks dependencies only)
-- | These are issues that must be closed before this one can start
getBlockers :: Issue -> Array IssueId
getBlockers issue =
  issue.dependencies
    # filter isBlockingDep
    # map _.dependsOnId
  where
  isBlockingDep :: Dependency -> Boolean
  isBlockingDep dep = case dep.depType of
    Blocks -> true
    ParentChild -> true  -- parent-child also blocks
    _ -> false

-- | Check if an issue has any open blockers
hasOpenBlockers :: IssueGraph -> Issue -> Boolean
hasOpenBlockers graph issue =
  getBlockers issue
    # map (\bid -> Map.lookup bid graph)
    # anyOpen
  where
  anyOpen :: Array (Maybe Issue) -> Boolean
  anyOpen arr = not $ all (not <<< isOpenMaybe) arr

  isOpenMaybe :: Maybe Issue -> Boolean
  isOpenMaybe Nothing = false  -- unknown issue doesn't block
  isOpenMaybe (Just i) = isOpen i

-- | Check if an issue is ready to work on
-- | An issue is ready if:
-- | 1. It's open (not closed)
-- | 2. It has no open blockers
isReady :: IssueGraph -> Issue -> Boolean
isReady graph issue =
  isOpen issue && not (hasOpenBlockers graph issue)

-- | Find all issues that are ready to work on
-- | This is the key function for `bd ready`
ready :: IssueGraph -> Array Issue
ready graph =
  graph
    # Map.values
    # Array.fromFoldable
    # filter (isReady graph)

-- | Find all issues that are ready, sorted by priority
-- | Lower priority number = higher priority (P0 is most urgent)
readyByPriority :: IssueGraph -> Array Issue
readyByPriority graph =
  ready graph
    # sortBy (\a b -> compare a.priority b.priority)

-- | Get issues that are blocked (have open blockers)
blocked :: IssueGraph -> Array Issue
blocked graph =
  graph
    # Map.values
    # Array.fromFoldable
    # filter (\i -> isOpen i && hasOpenBlockers graph i)

-- | Get all open issues
openIssues :: IssueGraph -> Array Issue
openIssues graph =
  graph
    # Map.values
    # Array.fromFoldable
    # filter isOpen

-- | Get all closed issues
closedIssues :: IssueGraph -> Array Issue
closedIssues graph =
  graph
    # Map.values
    # Array.fromFoldable
    # filter isClosed

-- | Summary statistics for an issue graph
type GraphStats =
  { total :: Int
  , open :: Int
  , closed :: Int
  , ready :: Int
  , blocked :: Int
  }

-- | Calculate statistics for an issue graph
stats :: IssueGraph -> GraphStats
stats graph =
  { total: Map.size graph
  , open: length $ openIssues graph
  , closed: length $ closedIssues graph
  , ready: length $ ready graph
  , blocked: length $ blocked graph
  }
