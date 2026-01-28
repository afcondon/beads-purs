# beads-purs

A git-backed issue tracker written in PureScript. Every change auto-commits, so your issues sync when you `git push/pull`.

Inspired by [Steve Yegge's beads](https://github.com/steveyegge/beads) - reimagined with functional programming principles.

## Installation

Requires Node.js (v16+).

```bash
git clone https://github.com/afcondon/beads-purs.git
cd beads-purs

# Option 1: Run directly
./bin/bd help

# Option 2: Symlink to PATH
ln -s $(pwd)/bin/bd /usr/local/bin/bd
```

No build step needed - the bundle is pre-compiled.

## Quick Start

```bash
cd your-project
bd init                              # Creates .beads/issues.jsonl
bd create "Fix login bug" -p 1       # P1 = high priority
bd create "Add dark mode" -p 2       # P2 = normal (default)
bd create "Update docs" -p 3         # P3 = low

bd ready                             # Shows what to work on next
# [P1] bd-a1b2: Fix login bug
# [P2] bd-c3d4: Add dark mode
# [P3] bd-e5f6: Update docs

bd close bd-a1b2 "Fixed in commit abc123"
bd ready                             # Login bug gone, dark mode is next
```

## Commands

| Command | Description |
|---------|-------------|
| `bd init` | Initialize beads in current directory |
| `bd create "title"` | Create issue (`-p` priority, `-d` description) |
| `bd ready` | Show unblocked issues by priority |
| `bd list` | Show all open issues (`-s` search, `-n` limit) |
| `bd show <id>` | Show full issue details |
| `bd close <id> "reason"` | Close an issue |
| `bd dep add <a> <b>` | Make `a` blocked by `b` |
| `bd dep rm <a> <b>` | Remove dependency |
| `bd edit <id>` | Edit issue (`-t` title, `-p` priority) |
| `bd stats` | Show statistics |
| `bd help` | Show help |

## Dependencies (Blocking)

The killer feature: model which issues block others.

```bash
bd create "Build API" -p 1           # bd-api1
bd create "Build Frontend" -p 1      # bd-fe02
bd dep add bd-fe02 bd-api1           # Frontend blocked by API

bd ready
# [P1] bd-api1: Build API
# (frontend doesn't appear - it's blocked)

bd close bd-api1 "API complete"
bd ready
# [P1] bd-fe02: Build Frontend
# (now it appears)
```

## Priority Levels

| Level | Meaning |
|-------|---------|
| P0 | Critical - drop everything |
| P1 | High - do soon |
| P2 | Normal - default |
| P3 | Low - when time permits |
| P4 | Backlog - someday |

## Git Integration

Every write operation auto-commits:

```
beads: create bd-a1b2 - Fix login bug
beads: close bd-a1b2 - Fixed in commit abc123
beads: dep add bd-fe02 blocked by bd-api1
```

Your issues travel with your code. Push to share, pull to sync.

## File Format

Issues are stored in `.beads/issues.jsonl` - one JSON object per line:

```json
{"id":"bd-a1b2","title":"Fix bug","status":"open","priority":1,"dependencies":[],...}
```

It's just text. You can grep it, edit it, diff it.

## Why beads-purs?

- **Git-native**: Issues live with code, not in a separate system
- **Offline-first**: No server, no internet required
- **AI-friendly**: Plain text files that agents can read/write
- **Conflict-free**: Hash-based IDs prevent merge collisions
- **Functional core**: Pure transformations, effects at edges

## Using with Claude Code

Claude won't automatically use beads just because `.beads/` exists - you need to tell it. Add this to your project's `CLAUDE.md`:

```markdown
## Issue Tracking

This project uses beads for issue tracking. Commands: `bd help`

**Start of session:**
- Run `bd ready` to see unblocked issues by priority
- Pick the top item to work on

**During work:**
- `bd create "title" -p N` when you discover new work needed
- `bd dep add <a> <b>` if issue a is blocked by issue b
- `bd show <id>` to see full details of an issue

**End of session:**
- `bd close <id> "what you did"` for completed work
- All changes auto-commit to git

**Priority levels:** P0=critical, P1=high, P2=normal, P3=low, P4=backlog
```

This gives Claude enough context to use beads effectively during development sessions.

## Building from Source

If you want to modify beads-purs:

```bash
# Install PureScript toolchain
npm install -g spago purescript

# Build
spago build

# Bundle
spago bundle --bundle-type app --platform node --outfile bin/bd.js

# Run
spago run
```

## Architecture

```
CLI (Main.purs)
    ↓
App (effectful wrapper - load/save/git)
    ↓
Commands (pure: Store → Either Error Store)
    ↓
Queries (pure: Store → Array Issue)
    ↓
Store (immutable Map IssueId Issue)
    ↓
JSONL (parse/serialize)
```

All business logic is pure. Effects only happen at the edges.

## Compatibility

Compatible with the original beads JSONL format. Should be able to read issues created by Steve Yegge's Go implementation.

## License

MIT

## Credits

- Original concept: [Steve Yegge's beads](https://github.com/steveyegge/beads)
- Built with: [PureScript](https://www.purescript.org/)
