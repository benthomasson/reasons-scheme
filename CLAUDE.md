# CLAUDE.md

## What This Is

Chez Scheme port of ftl-reasons — a belief tracking system based on Doyle's 1979 Truth Maintenance System. R6RS library name: `(ftl reasons)`. Reads/writes the same `reasons.db` SQLite format as the Python version for full interoperability.

## Key Concepts

Same as ftl-reasons: nodes (IN/OUT), premises, SL justifications with antecedents and outlist, retraction cascades, restoration, nogoods with dependency-directed backtracking, entrenchment scoring, challenge/defend, supersede.

## Project Structure

```
lib/ftl/
  reasons.sls              # (ftl reasons) — records + network engine
  reasons/
    json.sls               # (ftl reasons json) — JSON parser/serializer
    sqlite.sls             # (ftl reasons sqlite) — SQLite3 FFI bindings
    storage.sls            # (ftl reasons storage) — SQLite persistence
    merkle.sls             # (ftl reasons merkle) — SHA-256 hash integrity
tests/
  run-tests.ss             # 64 tests covering all TMS features + storage
```

## Running Tests

```bash
scheme --libdirs lib --script tests/run-tests.ss
```

## Using the Library

```bash
scheme --libdirs lib
```

```scheme
(import (ftl reasons) (ftl reasons storage))

;; Create a network
(define net (make-network))
(network-add-node! net "a" "Premise A")
(network-add-node! net "b" "Derived B"
  (list (cons 'justifications
    (list (make-justification "SL" '("a") '() "" "")))))

;; Save to SQLite (same format as Python ftl-reasons)
(define db (storage-open "reasons.db"))
(storage-save db net)
(storage-close db)
```

## Requirements

- Chez Scheme 10.x (`scheme` command)
- libsqlite3 (system library on macOS)

## Design Decisions

- Uses `(chezscheme)` import for hash tables, FFI, and format — not strictly R6RS-portable
- Pure Scheme SHA-256 implementation (no C dependency beyond SQLite)
- Dependents sets use Chez hash tables for O(1) operations
- Metadata stored as alists (association lists) — idiomatic for small key-value maps
- Network operations use mutation (`set!`) since TMS propagation is inherently stateful
