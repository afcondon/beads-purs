# beads-purs

A PureScript port of [beads](https://github.com/steveyegge/beads), the distributed git-backed issue tracker for AI coding agents.

## Why?

The original beads is written in Go and reportedly "100% vibe coded". This port aims to:

1. **Understand the architecture** — Types make structure explicit
2. **Get a typed specification** — The domain is rich in ADTs
3. **Trust what runs on our code** — PureScript is safer
4. **Potentially extend** — Visualization with PSD3, etc.

## Status

**Milestone 1: Types + JSONL** (in progress)

- [x] Core types (Issue, Status, Priority, etc.)
- [x] JSON encode/decode
- [x] JSONL parsing/serialization
- [x] File system read/write
- [x] ID generation
- [ ] Roundtrip tests against Go version

## Building

```bash
spago build
```

## Project Structure

```
src/
└── Beads/
    ├── Core/
    │   ├── Types.purs      # Issue, Status, Priority, etc.
    │   ├── Issue.purs      # JSON codecs for Issue
    │   └── Id.purs         # ID generation and parsing
    └── Storage/
        ├── JSONL.purs      # JSONL format parsing
        └── FileSystem.purs # .beads/ directory handling
```

## Compatibility

Aims to be compatible with Go beads' JSONL format. Should be able to read/write `issues.jsonl` created by either implementation.

## Related

- [Original beads](https://github.com/steveyegge/beads)
- [Port plan](../PSD3-Repos/docs/kb/plans/beads-purescript-port.md)
