# beads-purs Cheatsheet

A git-backed issue tracker. Every write auto-commits.

## Quick Start

```bash
cd your-project
bd init              # Creates .beads/issues.jsonl
bd create "My task"  # Creates issue, returns ID like bd-a1b2
bd ready             # What should I work on?
bd close bd-a1b2 "Done!"
```

## Commands

| Command | Example | Notes |
|---------|---------|-------|
| `bd init` | `bd init` | Creates `.beads/` in current dir |
| `bd create` | `bd create "Fix bug" -p 1` | Priority 0-4 (0=urgent, 2=default) |
| `bd ready` | `bd ready -n 5` | Shows unblocked issues by priority |
| `bd list` | `bd list -s "auth"` | Shows open issues, -s to search |
| `bd show` | `bd show bd-a1b2` | Full issue details |
| `bd close` | `bd close bd-a1b2 "reason"` | Closes with reason |
| `bd dep add` | `bd dep add bd-a1 bd-b2` | a1 blocked by b2 |
| `bd dep rm` | `bd dep rm bd-a1 bd-b2` | Remove dependency |
| `bd edit` | `bd edit bd-a1 -p 0` | Change priority or title |
| `bd stats` | `bd stats` | Total/open/closed/ready/blocked |
| `bd help` | `bd help` | Show all commands |

## Priority Levels

| Level | Meaning | Use for |
|-------|---------|---------|
| P0 | Critical | Blocking everything, fix now |
| P1 | High | Important, do soon |
| P2 | Normal | Default, regular work |
| P3 | Low | Nice to have |
| P4 | Backlog | Someday/maybe |

## Dependencies

```bash
bd dep add bd-frontend bd-backend  # frontend blocked by backend
bd ready                            # frontend won't appear until backend closed
bd close bd-backend "API done"
bd ready                            # frontend now appears
```

## Workflow

```
1. bd ready          # Pick top item
2. Work on it
3. bd close <id> "what you did"
4. bd ready          # Next item
```

## Search

```bash
bd list -s "auth"    # Find issues with "auth" in title
bd list -n 50        # Show more results
```

## Edit

```bash
bd edit bd-a1b2 -t "New title"   # Change title
bd edit bd-a1b2 -p 0             # Escalate to P0
```

## Files

```
your-project/
└── .beads/
    └── issues.jsonl    # One JSON object per line
```

## Git Integration

Every `create`, `close`, `edit`, or `dep` auto-commits:
```
beads: create bd-a1b2 - Fix the login bug
beads: close bd-a1b2 - Implemented in commit abc123
beads: dep add bd-a1 blocked by bd-b2
```

Your issues sync when you `git push/pull`.

## Tips

- IDs are stable (hash-based like `bd-a1b2`)
- IDs never collide across machines
- JSONL is grep-friendly: `grep "login" .beads/issues.jsonl`
- Edit JSONL directly if needed (it's just JSON)
- `bd ready` only shows unblocked issues
- Blocked count in `bd stats` shows dependency bottlenecks

## Issue Format (JSONL)

```json
{"id":"bd-a1b2","title":"Fix bug","status":"open","priority":1,"dependencies":[...],...}
```

Status values: `open`, `in_progress`, `blocked`, `closed`, `tombstone`
