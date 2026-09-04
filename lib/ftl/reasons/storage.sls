(library (ftl reasons storage)
  (export storage-open storage-close storage-save storage-load)
  (import (chezscheme) (ftl reasons) (ftl reasons sqlite) (ftl reasons json))

  (define SCHEMA_VERSION "1.0")

  (define SCHEMA "
CREATE TABLE IF NOT EXISTS nodes (
    id TEXT PRIMARY KEY,
    text TEXT NOT NULL,
    truth_value TEXT NOT NULL DEFAULT 'IN',
    supporting_justification INTEGER DEFAULT NULL,
    source TEXT DEFAULT '',
    source_url TEXT DEFAULT '',
    source_hash TEXT DEFAULT '',
    text_hash TEXT DEFAULT '',
    date TEXT DEFAULT '',
    metadata_json TEXT DEFAULT '{}',
    created_at TEXT DEFAULT '',
    updated_at TEXT DEFAULT '',
    reviewed_at TEXT DEFAULT '',
    verified_at TEXT DEFAULT '',
    retracted_at TEXT DEFAULT ''
);

CREATE TABLE IF NOT EXISTS justifications (
    rowid INTEGER PRIMARY KEY AUTOINCREMENT,
    node_id TEXT NOT NULL REFERENCES nodes(id),
    type TEXT NOT NULL,
    antecedents_json TEXT NOT NULL DEFAULT '[]',
    outlist_json TEXT NOT NULL DEFAULT '[]',
    label TEXT DEFAULT '',
    content_hash TEXT DEFAULT ''
);

CREATE TABLE IF NOT EXISTS nogoods (
    id TEXT PRIMARY KEY,
    nodes_json TEXT NOT NULL DEFAULT '[]',
    discovered TEXT DEFAULT '',
    resolution TEXT DEFAULT ''
);

CREATE TABLE IF NOT EXISTS repos (
    name TEXT PRIMARY KEY,
    path TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS propagation_log (
    rowid INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    action TEXT NOT NULL,
    target TEXT NOT NULL,
    value TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS network_meta (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
);
")

  ;; FTS table created separately (can't be inside executescript-style multi-statement)
  (define FTS-SCHEMA
    "CREATE VIRTUAL TABLE IF NOT EXISTS nodes_fts USING fts5(id, text, tokenize='porter unicode61');")

  (define (storage-open db-path)
    (let* ([is-new (not (file-exists? db-path))]
           [db (sqlite-open db-path)])
      (sqlite-exec db "PRAGMA journal_mode=WAL")
      (sqlite-exec db "PRAGMA foreign_keys=ON")
      ;; Create tables
      (sqlite-exec db SCHEMA)
      (guard (e [#t (void)])
        (sqlite-exec db FTS-SCHEMA))
      ;; Init meta for new databases
      (when is-new
        (let ([now (current-iso8601)])
          (for-each
            (lambda (kv)
              (sqlite-execute db
                "INSERT OR IGNORE INTO network_meta (key, value) VALUES (?, ?)"
                (list (car kv) (cdr kv))))
            (list (cons "schema_version" SCHEMA_VERSION)
                  (cons "project_name" (path-stem db-path))
                  (cons "created_at" now)
                  (cons "updated_at" now)))))
      db))

  (define (path-stem path)
    (let ([base (let loop ([i (- (string-length path) 1)])
                  (cond
                    [(< i 0) path]
                    [(char=? (string-ref path i) #\/) (substring path (+ i 1) (string-length path))]
                    [else (loop (- i 1))]))])
      (let loop ([i (- (string-length base) 1)])
        (cond
          [(< i 0) base]
          [(char=? (string-ref base i) #\.) (substring base 0 i)]
          [else (loop (- i 1))]))))

  (define (storage-close db)
    (sqlite-close db))

  (define (storage-save db net)
    (sqlite-exec db "BEGIN TRANSACTION")
    (guard (e [#t (sqlite-exec db "ROLLBACK") (raise e)])
      ;; Clear existing data
      (sqlite-exec db "DELETE FROM justifications")
      (sqlite-exec db "DELETE FROM nodes")
      (guard (e [#t (void)])
        (sqlite-exec db "DROP TABLE IF EXISTS nodes_fts")
        (sqlite-exec db FTS-SCHEMA))
      (sqlite-exec db "DELETE FROM nogoods")
      (sqlite-exec db "DELETE FROM repos")
      (sqlite-exec db "DELETE FROM propagation_log")
      (sqlite-exec db "DELETE FROM network_meta")

      ;; Save nodes
      (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
        (vector-for-each
          (lambda (nid node)
            (sqlite-execute db
              "INSERT INTO nodes (id, text, truth_value, supporting_justification,
               source, source_url, source_hash, text_hash, date, metadata_json,
               created_at, updated_at, reviewed_at, verified_at, retracted_at)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
              (list (node-id node)
                    (node-text node)
                    (node-truth-value node)
                    (let ([sj (node-supporting-justification node)])
                      (if sj (number->string sj) #f))
                    (node-source node)
                    (node-source-url node)
                    (node-source-hash node)
                    (node-text-hash node)
                    (node-date node)
                    (json-write (node-metadata node))
                    (node-created-at node)
                    (node-updated-at node)
                    (node-reviewed-at node)
                    (node-verified-at node)
                    (node-retracted-at node)))
            ;; FTS
            (guard (e [#t (void)])
              (sqlite-execute db
                "INSERT INTO nodes_fts (id, text) VALUES (?, ?)"
                (list (node-id node) (node-text node))))
            ;; Justifications
            (for-each
              (lambda (j)
                (sqlite-execute db
                  "INSERT INTO justifications (node_id, type, antecedents_json, outlist_json, label, content_hash)
                   VALUES (?, ?, ?, ?, ?, ?)"
                  (list (node-id node)
                        (justification-type j)
                        (json-write (justification-antecedents j))
                        (json-write (justification-outlist j))
                        (justification-label j)
                        (justification-content-hash j))))
              (node-justifications node)))
          keys vals))

      ;; Save nogoods
      (for-each
        (lambda (ng)
          (sqlite-execute db
            "INSERT INTO nogoods (id, nodes_json, discovered, resolution)
             VALUES (?, ?, ?, ?)"
            (list (nogood-id ng)
                  (json-write (nogood-nodes ng))
                  (nogood-discovered ng)
                  (nogood-resolution ng))))
        (network-nogoods net))

      ;; Save meta
      (let ([now (current-iso8601)]
            [meta (network-meta net)])
        (let ([meta (if (assoc "schema_version" meta)
                        meta
                        (alist-set "schema_version" SCHEMA_VERSION meta))])
          (let ([meta (alist-set "updated_at" now meta)])
            (for-each
              (lambda (kv)
                (sqlite-execute db
                  "INSERT OR REPLACE INTO network_meta (key, value) VALUES (?, ?)"
                  (list (car kv) (if (string? (cdr kv)) (cdr kv) (format "~a" (cdr kv))))))
              meta))))

      ;; Save repos
      (for-each
        (lambda (kv)
          (sqlite-execute db
            "INSERT INTO repos (name, path) VALUES (?, ?)"
            (list (car kv) (cdr kv))))
        (network-repos net))

      ;; Save log
      (for-each
        (lambda (entry)
          (sqlite-execute db
            "INSERT INTO propagation_log (timestamp, action, target, value)
             VALUES (?, ?, ?, ?)"
            (list (alist-ref "timestamp" entry "")
                  (alist-ref "action" entry "")
                  (alist-ref "target" entry "")
                  (alist-ref "value" entry ""))))
        (network-log net))

      (sqlite-exec db "COMMIT")))

  (define (storage-load db)
    (let ([net (make-network)])
      ;; Load nodes (without justifications first)
      (let ([node-rows (sqlite-query db
              "SELECT id, text, truth_value, supporting_justification,
               source, source_url, source_hash, date, metadata_json,
               created_at, updated_at, reviewed_at, verified_at, retracted_at,
               text_hash FROM nodes")])
        ;; Load justifications keyed by node_id
        (let ([just-rows (sqlite-query db
                "SELECT node_id, type, antecedents_json, outlist_json, label, content_hash
                 FROM justifications ORDER BY rowid")]
              [justs-by-node (make-hashtable string-hash string=?)])
          (for-each
            (lambda (row)
              (let* ([node-id (list-ref row 0)]
                     [j (make-justification
                          (list-ref row 1)
                          (json-read (list-ref row 2))
                          (json-read (list-ref row 3))
                          (list-ref row 4)
                          (list-ref row 5))])
                (hashtable-update! justs-by-node node-id
                  (lambda (existing) (append existing (list j)))
                  '())))
            just-rows)

          ;; Build nodes directly (bypass network-add-node! to preserve exact state)
          (for-each
            (lambda (row)
              (let* ([nid (list-ref row 0)]
                     [text (list-ref row 1)]
                     [tv (list-ref row 2)]
                     [sj-str (list-ref row 3)]
                     [sj (if (or (string=? sj-str "") (string=? sj-str ""))
                             #f
                             (let ([n (string->number sj-str)])
                               (and n (exact? n) n)))]
                     [source (list-ref row 4)]
                     [source-url (list-ref row 5)]
                     [source-hash (list-ref row 6)]
                     [date (list-ref row 7)]
                     [meta-json (list-ref row 8)]
                     [created-at (list-ref row 9)]
                     [updated-at (list-ref row 10)]
                     [reviewed-at (list-ref row 11)]
                     [verified-at (list-ref row 12)]
                     [retracted-at (list-ref row 13)]
                     [text-hash (list-ref row 14)]
                     [justs (hashtable-ref justs-by-node nid '())]
                     [metadata (let ([parsed (json-read meta-json)])
                                 (if (and (list? parsed) (or (null? parsed) (pair? (car parsed))))
                                     parsed
                                     '()))]
                     [node (make-node nid text tv justs sj (string-set-empty)
                             source source-url source-hash text-hash date metadata
                             created-at updated-at reviewed-at verified-at retracted-at)])
                (hashtable-set! (network-nodes net) nid node)))
            node-rows)))

      ;; Rebuild dependents index
      (network-rebuild-dependents! net)

      ;; Load nogoods
      (let ([ng-rows (sqlite-query db
              "SELECT id, nodes_json, discovered, resolution FROM nogoods")])
        (network-nogoods-set! net
          (map (lambda (row)
                 (make-nogood (list-ref row 0)
                              (json-read (list-ref row 1))
                              (list-ref row 2)
                              (list-ref row 3)))
               ng-rows)))

      ;; Load meta
      (guard (e [#t (void)])
        (let ([meta-rows (sqlite-query db "SELECT key, value FROM network_meta")])
          (network-meta-set! net
            (map (lambda (row) (cons (list-ref row 0) (list-ref row 1)))
                 meta-rows))))

      ;; Load repos
      (guard (e [#t (void)])
        (let ([repo-rows (sqlite-query db "SELECT name, path FROM repos")])
          (network-repos-set! net
            (map (lambda (row) (cons (list-ref row 0) (list-ref row 1)))
                 repo-rows))))

      ;; Load log
      (let ([log-rows (sqlite-query db
              "SELECT timestamp, action, target, value FROM propagation_log ORDER BY rowid")])
        (network-log-set! net
          (map (lambda (row)
                 (list (cons "timestamp" (list-ref row 0))
                       (cons "action" (list-ref row 1))
                       (cons "target" (list-ref row 2))
                       (cons "value" (list-ref row 3))))
               log-rows)))

      net))

) ;; end library
