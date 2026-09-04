#!/usr/bin/env scheme-script
;; Test suite for (ftl reasons) — port of test_network.py, test_backtracking.py, test_storage.py

(import (chezscheme) (ftl reasons) (ftl reasons storage))

;; ---- Minimal test framework ----

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *failures* '())

(define-syntax test
  (syntax-rules ()
    [(_ name body ...)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (guard (e [#t (set! *tests-failed* (+ *tests-failed* 1))
                     (set! *failures* (cons (cons name (condition-message e)) *failures*))
                     (display (string-append "  FAIL: " name "\n"))
                     (display (string-append "    " (condition-message e) "\n"))])
         body ...
         (set! *tests-passed* (+ *tests-passed* 1))
         (display (string-append "  PASS: " name "\n"))))]))

(define (assert-equal actual expected msg)
  (unless (equal? actual expected)
    (error 'assert-equal
      (format "~a: expected ~s, got ~s" msg expected actual))))

(define (assert-true val msg)
  (unless val
    (error 'assert-true (format "~a: expected true, got ~s" msg val))))

(define (assert-false val msg)
  (unless (not val)
    (error 'assert-false (format "~a: expected false, got ~s" msg val))))

(define (assert-member item lst msg)
  (unless (member item lst)
    (error 'assert-member
      (format "~a: ~s not found in ~s" msg item lst))))

(define (assert-set-equal actual expected msg)
  (let ([a (list-sort string<? actual)]
        [e (list-sort string<? expected)])
    (unless (equal? a e)
      (error 'assert-set-equal
        (format "~a: expected ~s, got ~s" msg e a)))))

(define (assert-raises thunk msg)
  (guard (e [#t #t])
    (thunk)
    (error 'assert-raises (format "~a: expected exception" msg))))

(define (string-contains? s sub)
  (let ([slen (string-length s)]
        [sublen (string-length sub)])
    (let loop ([i 0])
      (cond
        [(> (+ i sublen) slen) #f]
        [(string=? (substring s i (+ i sublen)) sub) #t]
        [else (loop (+ i 1))]))))

(define (node-tv net id)
  (node-truth-value (hashtable-ref (network-nodes net) id #f)))

(define (j1 ants) (make-justification "SL" ants '() "" ""))
(define (j1-out ants outs) (make-justification "SL" ants outs "" ""))
(define (j1-lbl ants lbl) (make-justification "SL" ants '() lbl ""))

;; ---- TestAddNode ----

(display "\n=== TestAddNode ===\n")

(test "add_premise"
  (let ([net (make-network)])
    (let ([node (network-add-node! net "a" "Premise A")])
      (assert-equal (node-truth-value node) "IN" "truth value")
      (assert-equal (node-justifications node) '() "justifications"))))

(test "add_derived_node"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (let ([node (network-add-node! net "b" "Derived B"
                  (list (cons 'justifications (list (j1 '("a"))))))])
      (assert-equal (node-truth-value node) "IN" "truth value"))))

(test "add_derived_node_antecedent_out"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-retract! net "a")
    (let ([node (network-add-node! net "b" "Derived B"
                  (list (cons 'justifications (list (j1 '("a"))))))])
      (assert-equal (node-truth-value node) "OUT" "truth value"))))

(test "add_duplicate_raises"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (assert-raises (lambda () (network-add-node! net "a" "Duplicate"))
                   "duplicate")))

(test "dependents_registered"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (assert-true
      (string-set-contains? (node-dependents (hashtable-ref (network-nodes net) "a" #f)) "b")
      "b in a's dependents")))

;; ---- TestRetraction ----

(display "\n=== TestRetraction ===\n")

(test "retract_premise"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (let ([changed (network-retract! net "a")])
      (assert-equal (node-tv net "a") "OUT" "truth value")
      (assert-member "a" changed "changed"))))

(test "retract_cascades_to_dependent"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (let ([changed (network-retract! net "a")])
      (assert-equal (node-tv net "b") "OUT" "b truth value")
      (assert-member "b" changed "changed"))))

(test "retract_cascades_through_chain"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("b"))))))
    (let ([changed (network-retract! net "a")])
      (assert-equal (node-tv net "b") "OUT" "b")
      (assert-equal (node-tv net "c") "OUT" "c")
      (assert-set-equal changed '("a" "b" "c") "changed"))))

(test "retract_already_out_is_noop"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-retract! net "a")
    (let ([changed (network-retract! net "a")])
      (assert-equal changed '() "changed"))))

(test "retract_nonexistent_raises"
  (let ([net (make-network)])
    (assert-raises (lambda () (network-retract! net "missing")) "raises")))

(test "retract_does_not_cascade_with_alternate_justification"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "c" "Premise C")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications
              (list (j1 '("a")) (j1 '("c"))))))
    (let ([changed (network-retract! net "a")])
      (assert-equal (node-tv net "b") "IN" "b stays IN")
      (assert-false (member "b" changed) "b not in changed"))))

(test "retract_already_out_pins_retracted"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (assert-equal (node-tv net "b") "OUT" "b out after a retract")
    (let ([result (network-retract! net "b")])
      (assert-equal result '() "no state change")
      (assert-true (alist-ref "_retracted" (node-metadata (hashtable-ref (network-nodes net) "b" #f)) #f)
                   "_retracted set"))
    (network-assert-node! net "a")
    (assert-equal (node-tv net "a") "IN" "a restored")
    (assert-equal (node-tv net "b") "OUT" "b stays pinned")
    (network-recompute-all! net)
    (assert-equal (node-tv net "b") "OUT" "recompute respects pin")))

;; ---- TestRestoration ----

(display "\n=== TestRestoration ===\n")

(test "assert_restores_dependent"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (assert-equal (node-tv net "b") "OUT" "b out")
    (let ([changed (network-assert-node! net "a")])
      (assert-equal (node-tv net "a") "IN" "a in")
      (assert-equal (node-tv net "b") "IN" "b restored")
      (assert-set-equal changed '("a" "b") "changed"))))

(test "assert_restores_chain"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("b"))))))
    (network-retract! net "a")
    (assert-equal (node-tv net "c") "OUT" "c out")
    (network-assert-node! net "a")
    (assert-equal (node-tv net "b") "IN" "b restored")
    (assert-equal (node-tv net "c") "IN" "c restored")))

(test "assert_already_in_is_noop"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (let ([changed (network-assert-node! net "a")])
      (assert-equal changed '() "changed"))))

(test "assert_nonexistent_raises"
  (let ([net (make-network)])
    (assert-raises (lambda () (network-assert-node! net "missing")) "raises")))

;; ---- TestMultipleAntecedents ----

(display "\n=== TestMultipleAntecedents ===\n")

(test "sl_requires_all_antecedents"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Premise C")
    (network-add-node! net "d" "Derived D"
      (list (cons 'justifications (list (j1 '("a" "b" "c"))))))
    (assert-equal (node-tv net "d") "IN" "d in")
    (network-retract! net "b")
    (assert-equal (node-tv net "d") "OUT" "d out")
    (network-assert-node! net "b")
    (assert-equal (node-tv net "d") "IN" "d restored")))

;; ---- TestOutlist (non-monotonic) ----

(display "\n=== TestOutlist ===\n")

(test "outlist_basic"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Unless B"
      (list (cons 'justifications
              (list (j1-out '("a") '("b"))))))
    ;; b is IN, so c's justification is invalid (outlist violated)
    (assert-equal (node-tv net "c") "OUT" "c out because b in")
    ;; Retract b — c should come IN
    (network-retract! net "b")
    (assert-equal (node-tv net "c") "IN" "c in because b out")
    ;; Restore b — c goes back OUT
    (network-assert-node! net "b")
    (assert-equal (node-tv net "c") "OUT" "c out again")))

(test "outlist_only_justification"
  (let ([net (make-network)])
    (network-add-node! net "blocker" "Blocker")
    (network-add-node! net "default" "Default holds unless blocker"
      (list (cons 'justifications
              (list (j1-out '() '("blocker"))))))
    (assert-equal (node-tv net "default") "OUT" "blocked")
    (network-retract! net "blocker")
    (assert-equal (node-tv net "default") "IN" "unblocked")))

;; ---- TestNogood ----

(display "\n=== TestNogood ===\n")

(test "nogood_retracts_one"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "d" "Premise D")
    (network-add-nogood! net '("a" "d"))
    (let ([vals (list (node-tv net "a") (node-tv net "d"))])
      (assert-true (member "OUT" vals) "one is OUT")
      (assert-equal (length (network-nogoods net)) 1 "nogood count"))))

(test "nogood_recorded"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "d" "Premise D")
    (network-add-nogood! net '("a" "d"))
    (assert-equal (nogood-nodes (car (network-nogoods net))) '("a" "d") "nodes")
    (assert-equal (nogood-id (car (network-nogoods net))) "nogood-a-d" "id")))

(test "nogood_inactive_when_one_already_out"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "d" "Premise D")
    (network-retract! net "d")
    (let ([changed (network-add-nogood! net '("a" "d"))])
      (assert-equal changed '() "no retraction needed")
      (assert-equal (node-tv net "a") "IN" "a still in"))))

(test "nogood_nonexistent_raises"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (assert-raises (lambda () (network-add-nogood! net '("a" "missing"))) "raises")))

;; ---- TestExplain ----

(display "\n=== TestExplain ===\n")

(test "explain_premise_in"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (let ([steps (network-explain net "a")])
      (assert-equal (length steps) 1 "one step")
      (assert-equal (alist-ref "reason" (car steps) "") "premise" "reason")
      (assert-equal (alist-ref "truth_value" (car steps) "") "IN" "truth value"))))

(test "explain_premise_retracted"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-retract! net "a")
    (let ([steps (network-explain net "a")])
      (assert-equal (alist-ref "reason" (car steps) "") "retracted premise" "reason"))))

(test "explain_derived_in"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1-lbl '("a") "A supports B")))))
    (let ([steps (network-explain net "b")])
      (assert-equal (alist-ref "node" (car steps) "") "b" "node")
      (assert-equal (alist-ref "truth_value" (car steps) "") "IN" "truth value")
      (assert-equal (alist-ref "label" (car steps) "") "A supports B" "label")
      (assert-true (exists (lambda (s) (equal? (alist-ref "node" s "") "a")) steps)
                   "a in trace"))))

(test "explain_chain"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("b"))))))
    (let* ([steps (network-explain net "c")]
           [nodes (map (lambda (s) (alist-ref "node" s "")) steps)])
      (assert-member "c" nodes "c in trace")
      (assert-member "b" nodes "b in trace")
      (assert-member "a" nodes "a in trace"))))

(test "explain_derived_out"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (let ([steps (network-explain net "b")])
      (assert-equal (alist-ref "truth_value" (car steps) "") "OUT" "out")
      (assert-member "a" (alist-ref "failed_antecedents" (car steps) '()) "failed"))))

(test "explain_nonexistent_raises"
  (let ([net (make-network)])
    (assert-raises (lambda () (network-explain net "missing")) "raises")))

;; ---- TestBeliefSet ----

(display "\n=== TestBeliefSet ===\n")

(test "belief_set"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (assert-set-equal (network-get-belief-set net) '("a" "b" "c") "all in")
    (network-retract! net "a")
    (assert-set-equal (network-get-belief-set net) '("b") "just b")))

;; ---- TestLog ----

(display "\n=== TestLog ===\n")

(test "log_records_add"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (assert-true
      (exists (lambda (e)
                (and (equal? (alist-ref "action" e "") "add")
                     (equal? (alist-ref "target" e "") "a")))
              (network-log net))
      "add logged")))

(test "log_records_retract_and_propagate"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (let ([actions (map (lambda (e)
                          (cons (alist-ref "action" e "")
                                (alist-ref "target" e "")))
                        (network-log net))])
      (assert-true (member (cons "retract" "a") actions) "retract logged")
      (assert-true (member (cons "propagate" "b") actions) "propagate logged"))))

;; ---- TestDiamondDependency ----

(display "\n=== TestDiamondDependency ===\n")

(test "diamond_retract_and_restore"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "d" "Derived D"
      (list (cons 'justifications (list (j1 '("b" "c"))))))
    (assert-equal (node-tv net "d") "IN" "d in")
    (network-retract! net "a")
    (assert-equal (node-tv net "b") "OUT" "b out")
    (assert-equal (node-tv net "c") "OUT" "c out")
    (assert-equal (node-tv net "d") "OUT" "d out")
    (network-assert-node! net "a")
    (assert-equal (node-tv net "b") "IN" "b restored")
    (assert-equal (node-tv net "c") "IN" "c restored")
    (assert-equal (node-tv net "d") "IN" "d restored")))

(test "diamond_with_retracted_intermediate"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "d" "Derived D"
      (list (cons 'justifications (list (j1 '("b" "c"))))))
    ;; Explicitly retract B — sticky
    (network-retract! net "b")
    (assert-equal (node-tv net "b") "OUT" "b out")
    (assert-equal (node-tv net "d") "OUT" "d out")
    ;; Retract and restore A
    (network-retract! net "a")
    (assert-equal (node-tv net "c") "OUT" "c out")
    (network-assert-node! net "a")
    (assert-equal (node-tv net "c") "IN" "c restored")
    (assert-equal (node-tv net "b") "OUT" "b stays retracted")
    (assert-equal (node-tv net "d") "OUT" "d stays out")
    ;; Recompute should not resurrect B
    (network-recompute-all! net)
    (assert-equal (node-tv net "b") "OUT" "recompute respects retraction")
    (assert-equal (node-tv net "d") "OUT" "d still out")
    ;; Explicitly asserting B clears retraction
    (network-assert-node! net "b")
    (assert-equal (node-tv net "b") "IN" "b back")
    (assert-equal (node-tv net "d") "IN" "d back")))

;; ---- TestDanglingDependents ----

(display "\n=== TestDanglingDependents ===\n")

(test "propagate_skips_dangling_dependent"
  (let ([net (make-network)])
    (network-add-node! net "a" "premise A")
    (string-set-add! (node-dependents (hashtable-ref (network-nodes net) "a" #f)) "nonexistent")
    (network-retract! net "a")))

(test "propagate_logs_dangling_warning"
  (let ([net (make-network)])
    (network-add-node! net "a" "premise A")
    (string-set-add! (node-dependents (hashtable-ref (network-nodes net) "a" #f)) "ghost")
    (network-retract! net "a")
    (let ([warnings (filter (lambda (e) (equal? (alist-ref "action" e "") "warn"))
                            (network-log net))])
      (assert-equal (length warnings) 1 "one warning")
      (assert-equal (alist-ref "target" (car warnings) "") "ghost" "target")
      (assert-true (string-contains? (alist-ref "value" (car warnings) "") "dangling")
                   "dangling in value"))))

;; ---- TestTraceAssumptions (from test_backtracking.py) ----

(display "\n=== TestTraceAssumptions ===\n")

(test "premise_traces_to_itself"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (assert-equal (network-trace-assumptions net "a") '("a") "traces to self")))

(test "derived_traces_to_premise"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (assert-equal (network-trace-assumptions net "b") '("a") "traces to a")))

(test "chain_traces_to_root"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("b"))))))
    (assert-equal (network-trace-assumptions net "c") '("a") "traces to a")))

(test "diamond_traces_to_root"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "d" "Derived D"
      (list (cons 'justifications (list (j1 '("b" "c"))))))
    (assert-equal (network-trace-assumptions net "d") '("a") "traces to a")))

(test "multiple_premises"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a" "b"))))))
    (assert-set-equal (network-trace-assumptions net "c") '("a" "b") "a and b")))

(test "trace_nonexistent_raises"
  (let ([net (make-network)])
    (assert-raises (lambda () (network-trace-assumptions net "missing")) "raises")))

;; ---- TestFindCulprits ----

(display "\n=== TestFindCulprits ===\n")

(test "two_premises_contradicting"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (let* ([culprits (network-find-culprits net '("a" "b"))]
           [ids (map (lambda (c) (alist-ref "premise" c "")) culprits)])
      (assert-set-equal ids '("a" "b") "both culprits"))))

(test "derived_nodes_trace_to_premise"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (let* ([culprits (network-find-culprits net '("b" "c"))]
           [ids (map (lambda (c) (alist-ref "premise" c "")) culprits)])
      (assert-set-equal ids '("a" "b") "a and b"))))

(test "sorted_by_entrenchment"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "x" "X"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "y" "Y"
      (list (cons 'justifications (list (j1 '("a"))))))
    (let ([culprits (network-find-culprits net '("a" "b"))])
      (assert-equal (alist-ref "premise" (car culprits) "") "b"
                    "b first (fewer dependents)"))))

;; ---- TestBacktrackingInNogood ----

(display "\n=== TestBacktrackingInNogood ===\n")

(test "retracts_premise_not_derived"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-nogood! net '("b" "c"))
    (assert-equal (node-tv net "b") "OUT" "b retracted")
    (assert-equal (node-tv net "c") "IN" "c stays in")))

(test "shared_premise_resolves_both"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-nogood! net '("b" "c"))
    (assert-equal (node-tv net "a") "OUT" "a retracted")
    (assert-equal (node-tv net "b") "OUT" "b out")
    (assert-equal (node-tv net "c") "OUT" "c out")))

(test "backtracking_logs_culprit"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-nogood! net '("a" "b"))
    (let ([bt (filter (lambda (e) (equal? (alist-ref "action" e "") "backtrack"))
                      (network-log net))])
      (assert-equal (length bt) 1 "one backtrack")
      (assert-true (string-contains? (alist-ref "value" (car bt) "") "culprit")
                   "culprit in value"))))

(test "inactive_nogood_no_backtracking"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-retract! net "b")
    (let ([changed (network-add-nogood! net '("a" "b"))])
      (assert-equal changed '() "no change")
      (let ([bt (filter (lambda (e) (equal? (alist-ref "action" e "") "backtrack"))
                        (network-log net))])
        (assert-equal (length bt) 0 "no backtrack")))))

;; ---- TestEntrenchment ----

(display "\n=== TestEntrenchment ===\n")

(test "nogood_retracts_speculation_not_evidence"
  (let ([net (make-network)])
    (network-add-node! net "finite-tree-gap-closes"
      "Finite trees have spectral gap closing ~1/N"
      (list (cons 'source "physics-quantum-lattice:entries/combined-laplacian-results.md")))
    (network-add-node! net "w-is-bethe-lattice" "W dimension is a Bethe lattice")
    (network-add-node! net "tree-as-genealogy"
      "Picture A: the tree is the duplication genealogy"
      (list (cons 'justifications (list (j1 '("w-is-bethe-lattice"))))))
    (network-add-nogood! net '("tree-as-genealogy" "finite-tree-gap-closes"))
    (assert-equal (node-tv net "finite-tree-gap-closes") "IN" "evidence survives")
    (assert-true
      (or (equal? (node-tv net "tree-as-genealogy") "OUT")
          (equal? (node-tv net "w-is-bethe-lattice") "OUT"))
      "speculation retracted")))

;; ---- TestChallenge/Defend ----

(display "\n=== TestChallenge/Defend ===\n")

(test "challenge_makes_target_out"
  (let ([net (make-network)])
    (network-add-node! net "belief" "A belief")
    (let ([result (network-challenge! net "belief" "I disagree")])
      (assert-equal (node-tv net "belief") "OUT" "challenged goes out")
      (assert-equal (node-tv net (alist-ref "challenge_id" result "")) "IN" "challenge in"))))

(test "defend_restores_target"
  (let ([net (make-network)])
    (network-add-node! net "belief" "A belief")
    (let* ([cr (network-challenge! net "belief" "I disagree")]
           [cid (alist-ref "challenge_id" cr "")])
      (assert-equal (node-tv net "belief") "OUT" "challenged")
      (let ([dr (network-defend! net "belief" cid "Actually it's fine")])
        (assert-equal (node-tv net cid) "OUT" "challenge neutralized")
        (assert-equal (node-tv net "belief") "IN" "belief restored")))))

;; ---- TestSupersede ----

(display "\n=== TestSupersede ===\n")

(test "supersede_old_goes_out"
  (let ([net (make-network)])
    (network-add-node! net "old" "Old belief")
    (network-add-node! net "new" "New belief")
    (network-supersede! net "old" "new")
    (assert-equal (node-tv net "old") "OUT" "old out")
    (assert-equal (node-tv net "new") "IN" "new in")))

(test "supersede_reversible"
  (let ([net (make-network)])
    (network-add-node! net "old" "Old belief")
    (network-add-node! net "new" "New belief")
    (network-supersede! net "old" "new")
    (assert-equal (node-tv net "old") "OUT" "old out")
    (network-retract! net "new")
    (assert-equal (node-tv net "old") "IN" "old restored")))

;; ---- TestConvertToPremise ----

(display "\n=== TestConvertToPremise ===\n")

(test "convert_to_premise"
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (assert-equal (node-tv net "b") "OUT" "b out")
    (let ([result (network-convert-to-premise! net "b")])
      (assert-equal (node-tv net "b") "IN" "b now premise"))))

;; ---- TestSummarize ----

(display "\n=== TestSummarize ===\n")

(test "summarize_basic"
  (let ([net (make-network)])
    (network-add-node! net "a" "Claim A")
    (network-add-node! net "b" "Claim B")
    (let ([result (network-summarize! net "summary" "Summary of A and B" '("a" "b"))])
      (assert-equal (node-tv net "summary") "IN" "summary in")
      (network-retract! net "a")
      (assert-equal (node-tv net "summary") "OUT" "summary out"))))

;; ---- TestStorage (round-trip) ----

(display "\n=== TestStorage ===\n")

(define test-db-path "/tmp/reasons-test.db")

(define (cleanup-test-db)
  (when (file-exists? test-db-path)
    (delete-file test-db-path))
  (when (file-exists? (string-append test-db-path "-wal"))
    (delete-file (string-append test-db-path "-wal")))
  (when (file-exists? (string-append test-db-path "-shm"))
    (delete-file (string-append test-db-path "-shm"))))

(test "storage_round_trip_premise"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (node-tv loaded "a") "IN" "truth value preserved")
        (assert-equal (node-text (hashtable-ref (network-nodes loaded) "a" #f))
                      "Premise A" "text preserved"))))
  (cleanup-test-db))

(test "storage_round_trip_derived"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (node-tv loaded "a") "IN" "a in")
        (assert-equal (node-tv loaded "b") "IN" "b in")
        (assert-equal (length (node-justifications
                        (hashtable-ref (network-nodes loaded) "b" #f)))
                      1 "justification count")
        (assert-true
          (string-set-contains?
            (node-dependents (hashtable-ref (network-nodes loaded) "a" #f)) "b")
          "dependents rebuilt"))))
  (cleanup-test-db))

(test "storage_round_trip_retracted"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-retract! net "a")
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (node-tv loaded "a") "OUT" "a out")
        (assert-equal (node-tv loaded "b") "OUT" "b out"))))
  (cleanup-test-db))

(test "storage_round_trip_nogoods"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-nogood! net '("a" "b"))
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (length (network-nogoods loaded)) 1 "nogood count")
        (assert-equal (nogood-nodes (car (network-nogoods loaded)))
                      '("a" "b") "nogood nodes"))))
  (cleanup-test-db))

(test "storage_round_trip_outlist"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Premise B")
    (network-add-node! net "c" "Unless B"
      (list (cons 'justifications
              (list (j1-out '("a") '("b"))))))
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (node-tv loaded "c") "OUT" "c out because b in")
        (let* ([node-c (hashtable-ref (network-nodes loaded) "c" #f)]
               [j (car (node-justifications node-c))])
          (assert-equal (justification-outlist j) '("b") "outlist preserved")))))
  (cleanup-test-db))

(test "storage_round_trip_meta"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-meta-set! net (list (cons "project_name" "test-proj")))
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-true
          (assoc "project_name" (network-meta loaded))
          "meta key exists"))))
  (cleanup-test-db))

(test "storage_round_trip_log"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-retract! net "a")
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-true (> (length (network-log loaded)) 0) "log entries loaded")
        (assert-true
          (exists (lambda (e) (equal? (alist-ref "action" e "") "retract"))
                  (network-log loaded))
          "retract in log"))))
  (cleanup-test-db))

(test "storage_save_and_reload_preserves_cascade"
  (cleanup-test-db)
  (let ([net (make-network)])
    (network-add-node! net "a" "Premise A")
    (network-add-node! net "b" "Derived B"
      (list (cons 'justifications (list (j1 '("a"))))))
    (network-add-node! net "c" "Derived C"
      (list (cons 'justifications (list (j1 '("b"))))))
    (let ([db (storage-open test-db-path)])
      (storage-save db net)
      (let ([loaded (storage-load db)])
        (storage-close db)
        (assert-equal (node-tv loaded "c") "IN" "c in after load")
        (network-retract! loaded "a")
        (assert-equal (node-tv loaded "b") "OUT" "b cascade works")
        (assert-equal (node-tv loaded "c") "OUT" "c cascade works"))))
  (cleanup-test-db))

;; ---- Summary ----

(newline)
(display "==================================\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(unless (null? *failures*)
  (display "\nFailures:\n")
  (for-each (lambda (f)
              (display (format "  ~a: ~a\n" (car f) (cdr f))))
            (reverse *failures*)))
(display "==================================\n")

(when (> *tests-failed* 0)
  (exit 1))
