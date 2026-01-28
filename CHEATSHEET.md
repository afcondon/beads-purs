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
| `bd list` | `bd list -n 20` | Shows all open issues |
| `bd close` | `bd close bd-a1b2 "reason"` | Closes with reason |
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

## Workflow

```
1. bd ready          # Pick top item
2. Work on it
3. bd close <id> "what you did"
4. bd ready          # Next item
```

## Files

```
your-project/
└── .beads/
    └── issues.jsonl    # One JSON object per line
```

## Git Integration

Every `create`, `close`, or modification auto-commits:
```
beads: create bd-a1b2 - Fix the login bug
beads: close bd-a1b2 - Implemented in commit abc123
```

Your issues sync when you `git push/pull`.

## Tips

- IDs are stable (hash-based like `bd-a1b2`)
- IDs never collide across machines
- JSONL is grep-friendly: `grep "login" .beads/issues.jsonl`
- Edit JSONL directly if needed (it's just JSON)

## Coming Soon

- `bd dep add <from> <to>` - Make one issue block another
- `bd show <id>` - View full issue details
- `bd edit <id>` - Modify title/priority/description

## Issue Format (JSONL)

```json
{"id":"bd-a1b2","title":"Fix bug","status":"open","priority":1,...}
```

Status values: `open`, `in_progress`, `blocked`, `closed`, `tombstone`
