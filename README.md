# reasons-scheme

Chez Scheme implementation of Doyle's 1979 [Truth Maintenance System](https://en.wikipedia.org/wiki/Reason_maintenance) (TMS) -- a belief tracking engine that maintains consistency across a network of justified beliefs.

Port of [ftl-reasons](https://github.com/benthomasson/ftl-reasons) (Python). Reads and writes the same `reasons.db` SQLite format for full interoperability between the two implementations.

## Features

- **SL justifications** -- a node is IN iff all antecedents are IN and all outlist nodes are OUT
- **Non-monotonic reasoning** -- outlist enables default logic ("believe X unless Y")
- **BFS propagation cascades** -- retraction and restoration propagate through the dependency graph
- **Nogoods** -- contradiction sets with automatic dependency-directed backtracking
- **Entrenchment scoring** -- when resolving contradictions, retract the least-entrenched premise
- **Challenge/defend** -- dialectical argumentation on beliefs
- **Supersede** -- reversible replacement of one belief by another
- **Merkle hashes** -- SHA-256 integrity verification of node text and justification chains
- **SQLite persistence** -- save/load networks to the same schema used by the Python version

## Requirements

- [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.x
- libsqlite3 (system library on macOS, `apt install libsqlite3-dev` on Linux)

## Quick start

```bash
scheme --libdirs lib
```

```scheme
(import (ftl reasons) (ftl reasons storage))

;; Build a belief network
(define net (make-network))
(network-add-node! net "sky-is-blue" "The sky is blue")
(network-add-node! net "daytime" "It is daytime")
(network-add-node! net "sky-appears-blue" "The sky appears blue"
  (list (cons 'justifications
    (list (make-justification "SL" '("sky-is-blue" "daytime") '() "" "")))))

(node-truth-value (hashtable-ref (network-nodes net) "sky-appears-blue" #f))
;; => "IN"

;; Retract a premise -- dependents cascade to OUT
(network-retract! net "daytime")
(node-truth-value (hashtable-ref (network-nodes net) "sky-appears-blue" #f))
;; => "OUT"

;; Restore it -- dependents cascade back to IN
(network-assert-node! net "daytime")
(node-truth-value (hashtable-ref (network-nodes net) "sky-appears-blue" #f))
;; => "IN"

;; Non-monotonic reasoning: believe X unless Y
(network-add-node! net "stars-visible" "Stars are visible"
  (list (cons 'justifications
    (list (make-justification "SL" '() '("daytime") "" "")))))
;; stars-visible is OUT (daytime is IN, blocking via outlist)

(network-retract! net "daytime")
;; stars-visible flips to IN

;; Persist to SQLite (readable by Python ftl-reasons)
(define db (storage-open "reasons.db"))
(storage-save db net)
(storage-close db)
```

## Interoperability

The Scheme and Python implementations share the same SQLite schema. A database created by one can be read and modified by the other:

```bash
# Python writes
python -c "
from ftl_reasons import Network, Storage
net = Network()
net.add_node('hello', 'Hello from Python')
Storage.save(net, 'shared.db')
"

# Scheme reads
scheme --libdirs lib -q <<'EOF'
(import (ftl reasons) (ftl reasons storage))
(let* ([db (storage-open "shared.db")]
       [net (storage-load db)])
  (display (node-text (hashtable-ref (network-nodes net) "hello" #f)))
  (newline)
  (storage-close db))
EOF
;; => Hello from Python
```

## Running tests

```bash
scheme --libdirs lib --script tests/run-tests.ss
```

64 tests covering: node operations, retraction cascades, restoration, multiple antecedents, outlist non-monotonic reasoning, nogoods, dependency-directed backtracking, entrenchment, explain traces, assumption tracing, challenge/defend, supersede, convert-to-premise, summarize, diamond dependencies, dangling dependents, and storage round-trips.

## Project structure

```
lib/ftl/
  reasons.sls              (ftl reasons)         Core TMS engine
  reasons/
    json.sls               (ftl reasons json)    JSON parser/serializer
    sqlite.sls             (ftl reasons sqlite)  SQLite3 FFI bindings
    storage.sls            (ftl reasons storage)  SQLite persistence
    merkle.sls             (ftl reasons merkle)  SHA-256 hash integrity
tests/
  run-tests.ss             64 tests
```

## API reference

### Network operations

| Function | Description |
|---|---|
| `(make-network)` | Create an empty belief network |
| `(network-add-node! net id text [opts])` | Add a node (premise or justified) |
| `(network-retract! net id)` | Retract a node, cascading to dependents |
| `(network-assert-node! net id)` | Restore a retracted node, cascading restoration |
| `(network-explain net id)` | Recursive explanation trace |
| `(network-trace-assumptions net id)` | Find root premises supporting a node |
| `(network-find-culprits net node-ids)` | Find premises behind a contradiction, sorted by entrenchment |
| `(network-add-nogood! net node-ids)` | Declare a contradiction set; auto-resolves via backtracking |
| `(network-challenge! net id reason)` | Challenge a belief (makes it OUT) |
| `(network-defend! net id challenge-id reason)` | Defend against a challenge (restores belief) |
| `(network-supersede! net old-id new-id)` | Replace one belief with another (reversible) |
| `(network-convert-to-premise! net id)` | Convert a derived node to a premise |
| `(network-summarize! net id text source-ids)` | Create a summary node depending on sources |
| `(network-get-belief-set net)` | List all IN node IDs |
| `(network-recompute-all! net)` | Recompute truth values for all nodes |

### Storage

| Function | Description |
|---|---|
| `(storage-open path)` | Open (or create) a SQLite database |
| `(storage-save db net)` | Save a network to the database |
| `(storage-load db)` | Load a network from the database |
| `(storage-close db)` | Close the database |

## License

MIT
