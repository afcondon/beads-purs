module Beads.Core.Queries where

import Prelude

import Beads.Core.Store (Store)
import Beads.Core.Store as Store
import Beads.Core.Types (Issue, IssueId, Status(..), Dependency, DependencyType(..))
import Data.Array (filter, sortBy, length)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))

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

-- | Get all issues that this issue depends on (blocking dependencies only)
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
hasOpenBlockers :: Store -> Issue -> Boolean
hasOpenBlockers store issue =
  getBlockers issue
    # map (\bid -> Store.lookup bid store)
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
isReady :: Store -> Issue -> Boolean
isReady store issue =
  isOpen issue && not (hasOpenBlockers store issue)

-- | Find all issues that are ready to work on
-- | This is the key function for `bd ready`
ready :: Store -> Array Issue
ready store =
  Store.toArray store
    # filter (isReady store)

-- | Find all issues that are ready, sorted by priority
-- | Lower priority number = higher priority (P0 is most urgent)
readyByPriority :: Store -> Array Issue
readyByPriority store =
  ready store
    # sortBy (\a b -> compare a.priority b.priority)

-- | Get issues that are blocked (have open blockers)
blocked :: Store -> Array Issue
blocked store =
  Store.toArray store
    # filter (\i -> isOpen i && hasOpenBlockers store i)

-- | Get all open issues
openIssues :: Store -> Array Issue
openIssues store = Store.filter isOpen store

-- | Get all closed issues
closedIssues :: Store -> Array Issue
closedIssues store = Store.filter isClosed store

-- | Summary statistics
type Stats =
  { total :: Int
  , open :: Int
  , closed :: Int
  , ready :: Int
  , blocked :: Int
  }

-- | Calculate statistics for a store
stats :: Store -> Stats
stats store =
  { total: Store.size store
  , open: length $ openIssues store
  , closed: length $ closedIssues store
  , ready: length $ ready store
  , blocked: length $ blocked store
  }

-- | Search issues by title (case-insensitive substring match)
searchByTitle :: String -> Store -> Array Issue
searchByTitle query store =
  Store.filter (titleMatches query) store
  where
  titleMatches q issue =
    contains (toLower q) (toLower issue.title)

  toLower = identity  -- TODO: proper case folding
  contains needle haystack = true  -- TODO: proper substring search

-- | Find issues by label
byLabel :: String -> Store -> Array Issue
byLabel label store =
  Store.filter (hasLabel label) store
  where
  hasLabel l issue = l `elem` issue.labels
  elem x xs = filter (_ == x) xs /= []

-- | Find issues by assignee
byAssignee :: String -> Store -> Array Issue
byAssignee assignee store =
  Store.filter (isAssignedTo assignee) store
  where
  isAssignedTo a issue = issue.assignee == Just a

-- | Find issues by priority
byPriority :: Int -> Store -> Array Issue
byPriority p store =
  Store.filter (\i -> i.priority == p) store

-- | Find issues by status
byStatus :: Status -> Store -> Array Issue
byStatus s store =
  Store.filter (\i -> i.status == s) store
