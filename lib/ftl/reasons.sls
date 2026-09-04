(library (ftl reasons)
  (export
    ;; Justification record
    make-justification justification?
    justification-type justification-antecedents justification-outlist
    justification-label justification-content-hash
    justification-content-hash-set!

    ;; Node record
    make-node node?
    node-id node-text node-truth-value node-justifications
    node-supporting-justification node-dependents
    node-source node-source-url node-source-hash node-text-hash
    node-date node-metadata
    node-created-at node-updated-at node-reviewed-at
    node-verified-at node-retracted-at
    node-truth-value-set! node-justifications-set!
    node-supporting-justification-set! node-dependents-set!
    node-metadata-set! node-text-hash-set!
    node-updated-at-set! node-reviewed-at-set!
    node-verified-at-set! node-retracted-at-set!

    ;; Nogood record
    make-nogood nogood?
    nogood-id nogood-nodes nogood-discovered nogood-resolution
    nogood-resolution-set!

    ;; Network record
    make-network network?
    network-nodes network-nogoods network-repos network-log network-meta
    network-nogoods-set! network-repos-set! network-log-set! network-meta-set!

    ;; Network operations
    network-add-node!
    network-retract!
    network-assert-node!
    network-explain
    network-trace-assumptions
    network-find-culprits
    network-add-nogood!
    network-get-belief-set
    network-recompute-all!
    network-rebuild-dependents!
    network-verify-dependents
    network-add-justification!
    network-challenge!
    network-defend!
    network-supersede!
    network-convert-to-premise!
    network-remove-justification!
    network-summarize!

    ;; Helpers
    alist-ref alist-set alist-delete
    current-iso8601
    string-set-add! string-set-remove! string-set-contains?
    string-set->list string-set-empty)
  (import (chezscheme))

  ;; ---- Alist helpers ----

  (define (alist-ref key alist default)
    (let ([pair (assoc key alist)])
      (if pair (cdr pair) default)))

  (define (alist-set key value alist)
    (cons (cons key value)
          (remp (lambda (p) (equal? (car p) key)) alist)))

  (define (alist-delete key alist)
    (remp (lambda (p) (equal? (car p) key)) alist))

  ;; ---- String set (hashtable wrapper) ----

  (define string-set-empty
    (lambda () (make-hashtable string-hash string=?)))

  (define (string-set-add! s key)
    (hashtable-set! s key #t))

  (define (string-set-remove! s key)
    (hashtable-delete! s key))

  (define (string-set-contains? s key)
    (hashtable-contains? s key))

  (define (string-set->list s)
    (vector->list (hashtable-keys s)))

  ;; ---- Timestamp ----

  (define (pad2 n)
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))

  (define (pad4 n)
    (cond
      [(< n 10) (string-append "000" (number->string n))]
      [(< n 100) (string-append "00" (number->string n))]
      [(< n 1000) (string-append "0" (number->string n))]
      [else (number->string n)]))

  (define (current-iso8601)
    (let ([t (current-time)])
      (let ([secs (time-second t)])
        (let* ([days-since-epoch (div secs 86400)]
               [time-of-day (mod secs 86400)]
               [hours (div time-of-day 3600)]
               [minutes (div (mod time-of-day 3600) 60)]
               [seconds (mod time-of-day 60)])
          (let-values ([(year month day) (epoch-days->ymd days-since-epoch)])
            (string-append
              (pad4 year) "-" (pad2 month) "-" (pad2 day)
              "T" (pad2 hours) ":" (pad2 minutes) ":" (pad2 seconds)
              "+00:00"))))))

  (define (epoch-days->ymd days)
    (let* ([a (+ days 719468)]
           [era (div (if (>= a 0) a (- a 146096)) 146097)]
           [doe (- a (* era 146097))]
           [yoe (div (- doe (div doe 1460) (- (div doe 36524)) (div doe 146096)) 365)]
           [y (+ yoe (* era 400))]
           [doy (- doe (- (* 365 yoe) (div yoe 4) (- (div yoe 100))))]
           [mp (div (+ (* 5 doy) 2) 153)]
           [d (+ (- doy (div (+ (* 153 mp) 2) 5)) 1)]
           [m (+ mp (if (< mp 10) 3 -9))]
           [y (if (<= m 2) (+ y 1) y)])
      (values y m d)))

  ;; ---- Records ----

  (define-record-type justification
    (fields
      (immutable type)
      (immutable antecedents)
      (immutable outlist)
      (immutable label)
      (mutable content-hash)))

  (define-record-type node
    (fields
      (immutable id)
      (immutable text)
      (mutable truth-value)
      (mutable justifications)
      (mutable supporting-justification)
      (mutable dependents)
      (immutable source)
      (immutable source-url)
      (immutable source-hash)
      (mutable text-hash)
      (immutable date)
      (mutable metadata)
      (immutable created-at)
      (mutable updated-at)
      (mutable reviewed-at)
      (mutable verified-at)
      (mutable retracted-at)))

  (define-record-type nogood
    (fields
      (immutable id)
      (immutable nodes)
      (immutable discovered)
      (mutable resolution)))

  (define-record-type network
    (protocol
      (lambda (new)
        (case-lambda
          [() (new (make-hashtable string-hash string=?) '() '() '() '())]
          [(nodes nogoods repos log meta) (new nodes nogoods repos log meta)])))
    (fields
      (immutable nodes)
      (mutable nogoods)
      (mutable repos)
      (mutable log)
      (mutable meta)))

  ;; ---- Internal helpers ----

  (define (log-event! net action target value)
    (network-log-set! net
      (append (network-log net)
              (list (list (cons "timestamp" (current-iso8601))
                          (cons "action" action)
                          (cons "target" target)
                          (cons "value" value))))))

  (define (justification-valid? net j)
    (let ([type (justification-type j)])
      (if (or (string=? type "SL") (string=? type "CP"))
          (and (for-all
                 (lambda (a)
                   (let ([n (hashtable-ref (network-nodes net) a #f)])
                     (and n (string=? (node-truth-value n) "IN"))))
                 (justification-antecedents j))
               (for-all
                 (lambda (o)
                   (let ([n (hashtable-ref (network-nodes net) o #f)])
                     (or (not n) (string=? (node-truth-value n) "OUT"))))
                 (justification-outlist j)))
          #f)))

  (define (compute-truth net node)
    (let ([justs (node-justifications node)])
      (if (null? justs)
          (node-truth-value node)
          (let loop ([i (- (length justs) 1)])
            (cond
              [(< i 0)
               (node-supporting-justification-set! node #f)
               "OUT"]
              [(justification-valid? net (list-ref justs i))
               (node-supporting-justification-set! node i)
               "IN"]
              [else (loop (- i 1))])))))

  (define (propagate! net changed-id)
    (let ([changed '()]
          [queue (list changed-id)]
          [visited (string-set-empty)])
      (string-set-add! visited changed-id)
      (let loop ()
        (if (null? queue)
            (reverse changed)
            (let* ([current-id (car queue)]
                   [current (hashtable-ref (network-nodes net) current-id #f)])
              (set! queue (cdr queue))
              (when current
                (for-each
                  (lambda (dep-id)
                    (unless (string-set-contains? visited dep-id)
                      (let ([dep (hashtable-ref (network-nodes net) dep-id #f)])
                        (cond
                          [(not dep)
                           (log-event! net "warn" dep-id
                             (string-append "dangling dependent of " current-id))]
                          [(alist-ref "_retracted" (node-metadata dep) #f)
                           (void)]
                          [else
                           (let ([old-value (node-truth-value dep)]
                                 [new-value (compute-truth net dep)])
                             (unless (string=? old-value new-value)
                               (node-truth-value-set! dep new-value)
                               (set! changed (cons dep-id changed))
                               (log-event! net "propagate" dep-id new-value)
                               (string-set-add! visited dep-id)
                               (set! queue (append queue (list dep-id)))))]))))
                  (string-set->list (node-dependents current))))
              (loop))))))

  (define (entrenchment net node-id)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (if (not node)
          0
          (let ([score 0])
            (when (null? (node-justifications node))
              (set! score (+ score 100)))
            (when (and (string? (node-source node))
                       (> (string-length (node-source node)) 0))
              (set! score (+ score 50)))
            (when (and (string? (node-source-hash node))
                       (> (string-length (node-source-hash node)) 0))
              (set! score (+ score 25)))
            (set! score (+ score (* (length (string-set->list (node-dependents node))) 10)))
            (let ([btype (string-upcase
                           (alist-ref "beliefs_type" (node-metadata node) ""))])
              (set! score
                (+ score
                   (cond
                     [(or (string=? btype "AXIOM") (string=? btype "WARNING")) 90]
                     [(string=? btype "OBSERVATION") 80]
                     [(string=? btype "DERIVED") 40]
                     [(string=? btype "PREDICTED") 30]
                     [(string=? btype "NOTE") 10]
                     [else 20]))))
            score))))

  ;; ---- Public Network operations ----

  (define (network-add-node! net id text . opts)
    (let ([options (if (null? opts) '() (car opts))])
      (when (hashtable-ref (network-nodes net) id #f)
        (error 'network-add-node! (string-append "Node '" id "' already exists")))
      (let* ([now (current-iso8601)]
             [justs (alist-ref 'justifications options '())]
             [source (alist-ref 'source options "")]
             [source-url (alist-ref 'source-url options "")]
             [source-hash (alist-ref 'source-hash options "")]
             [date (alist-ref 'date options "")]
             [metadata (alist-ref 'metadata options '())]
             [created-at (let ([v (alist-ref 'created-at options "")])
                           (if (and (string? v) (> (string-length v) 0)) v now))]
             [updated-at (let ([v (alist-ref 'updated-at options "")])
                           (if (and (string? v) (> (string-length v) 0)) v now))]
             [reviewed-at (alist-ref 'reviewed-at options "")]
             [verified-at (alist-ref 'verified-at options "")]
             [retracted-at (alist-ref 'retracted-at options "")]
             [deps (string-set-empty)]
             [node ((record-constructor (record-constructor-descriptor node))
                    id text "IN" justs #f deps
                    source source-url source-hash "" date metadata
                    created-at updated-at reviewed-at verified-at retracted-at)])
        ;; Register as dependent of antecedents and outlist nodes
        (for-each
          (lambda (j)
            (for-each
              (lambda (ant-id)
                (let ([ant (hashtable-ref (network-nodes net) ant-id #f)])
                  (when ant (string-set-add! (node-dependents ant) id))))
              (justification-antecedents j))
            (for-each
              (lambda (out-id)
                (let ([out (hashtable-ref (network-nodes net) out-id #f)])
                  (when out (string-set-add! (node-dependents out) id))))
              (justification-outlist j)))
          justs)
        (hashtable-set! (network-nodes net) id node)
        ;; Compute initial truth value
        (if (null? justs)
            (node-truth-value-set! node "IN")
            (node-truth-value-set! node (compute-truth net node)))
        (log-event! net "add" id (node-truth-value node))
        node)))

  (define (network-retract! net node-id . opts)
    (let ([reason (if (null? opts) "" (car opts))])
      (let ([node (hashtable-ref (network-nodes net) node-id #f)])
        (unless node
          (error 'network-retract! (string-append "Node '" node-id "' not found")))
        (let ([now (current-iso8601)])
          (if (string=? (node-truth-value node) "OUT")
              (begin
                (node-metadata-set! node (alist-set "_retracted" #t (node-metadata node)))
                (node-retracted-at-set! node now)
                (node-updated-at-set! node now)
                '())
              (begin
                (node-truth-value-set! node "OUT")
                (node-metadata-set! node (alist-set "_retracted" #t (node-metadata node)))
                (node-retracted-at-set! node now)
                (node-updated-at-set! node now)
                (when (> (string-length reason) 0)
                  (node-metadata-set! node
                    (alist-set "retract_reason" reason (node-metadata node))))
                (log-event! net "retract" node-id (if (> (string-length reason) 0) reason "OUT"))
                (cons node-id (propagate! net node-id))))))))

  (define (network-assert-node! net node-id)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (unless node
        (error 'network-assert-node! (string-append "Node '" node-id "' not found")))
      (if (string=? (node-truth-value node) "IN")
          '()
          (begin
            (node-truth-value-set! node "IN")
            (node-metadata-set! node (alist-delete "_retracted" (node-metadata node)))
            (node-retracted-at-set! node "")
            (node-updated-at-set! node (current-iso8601))
            (log-event! net "assert" node-id "IN")
            (cons node-id (propagate! net node-id))))))

  (define (network-rebuild-dependents! net)
    (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
      (vector-for-each (lambda (k v) (node-dependents-set! v (string-set-empty))) keys vals)
      (vector-for-each
        (lambda (k node)
          (for-each
            (lambda (j)
              (for-each
                (lambda (ant-id)
                  (let ([ant (hashtable-ref (network-nodes net) ant-id #f)])
                    (when ant (string-set-add! (node-dependents ant) (node-id node)))))
                (justification-antecedents j))
              (for-each
                (lambda (out-id)
                  (let ([out (hashtable-ref (network-nodes net) out-id #f)])
                    (when out (string-set-add! (node-dependents out) (node-id node)))))
                (justification-outlist j)))
            (node-justifications node)))
        keys vals)))

  (define (network-verify-dependents net)
    (let ([expected (make-hashtable string-hash string=?)]
          [errors '()])
      (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
        (vector-for-each (lambda (k v) (hashtable-set! expected k (string-set-empty))) keys vals)
        (vector-for-each
          (lambda (k node)
            (for-each
              (lambda (j)
                (for-each
                  (lambda (ant-id)
                    (when (hashtable-ref expected ant-id #f)
                      (string-set-add! (hashtable-ref expected ant-id #f) (node-id node))))
                  (justification-antecedents j))
                (for-each
                  (lambda (out-id)
                    (when (hashtable-ref expected out-id #f)
                      (string-set-add! (hashtable-ref expected out-id #f) (node-id node))))
                  (justification-outlist j)))
              (node-justifications node)))
          keys vals)
        (vector-for-each
          (lambda (k node)
            (let ([live (string-set->list (node-dependents node))]
                  [exp (string-set->list (hashtable-ref expected k (string-set-empty)))])
              (let ([live-set (make-hashtable string-hash string=?)]
                    [exp-set (make-hashtable string-hash string=?)])
                (for-each (lambda (x) (hashtable-set! live-set x #t)) live)
                (for-each (lambda (x) (hashtable-set! exp-set x #t)) exp)
                (for-each
                  (lambda (x)
                    (unless (hashtable-ref exp-set x #f)
                      (set! errors (cons (string-append k ": extra dependent " x) errors))))
                  live)
                (for-each
                  (lambda (x)
                    (unless (hashtable-ref live-set x #f)
                      (set! errors (cons (string-append k ": missing dependent " x) errors))))
                  exp))))
          keys vals))
      (reverse errors)))

  (define (network-explain net node-id . rest)
    (let ([visited (if (and (not (null? rest)))
                       (car rest)
                       (string-set-empty))]
          [path (if (and (>= (length rest) 2))
                    (cadr rest)
                    (string-set-empty))])
      (let ([node (hashtable-ref (network-nodes net) node-id #f)])
        (unless node
          (error 'network-explain (string-append "Node '" node-id "' not found")))
        (cond
          [(string-set-contains? path node-id)
           (list (list (cons "node" node-id)
                       (cons "truth_value" (node-truth-value node))
                       (cons "reason" "circular dependency")))]
          [(string-set-contains? visited node-id) '()]
          [else
           (string-set-add! visited node-id)
           (let ([new-path (let ([p (string-set-empty)])
                             (for-each (lambda (x) (string-set-add! p x))
                                       (string-set->list path))
                             (string-set-add! p node-id)
                             p)])
             (if (null? (node-justifications node))
                 ;; Premise
                 (list (list (cons "node" node-id)
                             (cons "truth_value" (node-truth-value node))
                             (cons "reason"
                               (if (string=? (node-truth-value node) "IN")
                                   "premise"
                                   "retracted premise"))))
                 ;; Derived node
                 (if (string=? (node-truth-value node) "IN")
                     ;; Find valid justification
                     (let ([j (find-valid-justification net node)])
                       (if j
                           (let ([step (list (cons "node" node-id)
                                            (cons "truth_value" "IN")
                                            (cons "reason"
                                              (string-append (justification-type j)
                                                             " justification valid"))
                                            (cons "antecedents"
                                              (justification-antecedents j))
                                            (cons "label" (justification-label j)))])
                             (let ([step (if (null? (justification-outlist j))
                                            step
                                            (append step
                                              (list (cons "outlist"
                                                      (justification-outlist j)))))])
                               (append
                                 (list step)
                                 (apply append
                                   (map (lambda (ant-id)
                                          (network-explain net ant-id
                                            visited new-path))
                                        (justification-antecedents j))))))
                           '()))
                     ;; All justifications invalid
                     (apply append
                       (map (lambda (j)
                              (let ([failed (filter
                                              (lambda (a)
                                                (let ([n (hashtable-ref (network-nodes net) a #f)])
                                                  (and n (string=? (node-truth-value n) "OUT"))))
                                              (justification-antecedents j))]
                                    [violated (filter
                                                (lambda (o)
                                                  (let ([n (hashtable-ref (network-nodes net) o #f)])
                                                    (and n (string=? (node-truth-value n) "IN"))))
                                                (justification-outlist j))])
                                (let ([step (list (cons "node" node-id)
                                                  (cons "truth_value" "OUT")
                                                  (cons "reason"
                                                    (string-append (justification-type j)
                                                                   " justification invalid"))
                                                  (cons "failed_antecedents" failed)
                                                  (cons "label" (justification-label j)))])
                                  (list (if (null? violated)
                                            step
                                            (append step
                                              (list (cons "violated_outlist" violated))))))))
                            (node-justifications node))))))]))))

  (define (find-valid-justification net node)
    (let ([justs (node-justifications node)]
          [si (node-supporting-justification node)])
      (or (and si (>= si 0) (< si (length justs))
               (let ([candidate (list-ref justs si)])
                 (and (justification-valid? net candidate) candidate)))
          (let loop ([i (- (length justs) 1)])
            (cond
              [(< i 0) #f]
              [(justification-valid? net (list-ref justs i))
               (list-ref justs i)]
              [else (loop (- i 1))])))))

  (define (network-trace-assumptions net node-id)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (unless node
        (error 'network-trace-assumptions
          (string-append "Node '" node-id "' not found")))
      (let ([premises '()]
            [visited (string-set-empty)])
        (let walk ([nid node-id])
          (unless (or (string-set-contains? visited nid)
                      (not (hashtable-ref (network-nodes net) nid #f)))
            (string-set-add! visited nid)
            (let ([n (hashtable-ref (network-nodes net) nid #f)])
              (if (null? (node-justifications n))
                  (unless (member nid premises)
                    (set! premises (append premises (list nid))))
                  (for-each
                    (lambda (j)
                      (for-each walk (justification-antecedents j)))
                    (node-justifications n))))))
        premises)))

  (define (network-find-culprits net nogood-node-ids)
    (let ([assumptions-by-node (make-hashtable string-hash string=?)]
          [all-premises (string-set-empty)])
      (for-each
        (lambda (nid)
          (let ([node (hashtable-ref (network-nodes net) nid #f)])
            (when (and node (string=? (node-truth-value node) "IN"))
              (let ([assumptions (network-trace-assumptions net nid)])
                (hashtable-set! assumptions-by-node nid assumptions)
                (for-each (lambda (a) (string-set-add! all-premises a))
                          assumptions)))))
        nogood-node-ids)
      (let ([candidates '()])
        (for-each
          (lambda (premise-id)
            (let ([would-resolve '()])
              (let-values ([(keys vals) (hashtable-entries assumptions-by-node)])
                (vector-for-each
                  (lambda (nid assumptions)
                    (when (member premise-id assumptions)
                      (set! would-resolve (cons nid would-resolve))))
                  keys vals))
              (when (not (null? would-resolve))
                (let ([ent (entrenchment net premise-id)]
                      [dep-count (length
                                   (string-set->list
                                     (node-dependents
                                       (hashtable-ref (network-nodes net) premise-id #f))))])
                  (set! candidates
                    (cons (list (cons "premise" premise-id)
                                (cons "would_resolve" (reverse would-resolve))
                                (cons "dependent_count" dep-count)
                                (cons "entrenchment" ent))
                          candidates))))))
          (string-set->list all-premises))
        (list-sort
          (lambda (a b)
            (< (alist-ref "entrenchment" a 0)
               (alist-ref "entrenchment" b 0)))
          (reverse candidates)))))

  (define (network-add-nogood! net node-ids)
    (for-each
      (lambda (nid)
        (unless (hashtable-ref (network-nodes net) nid #f)
          (error 'network-add-nogood!
            (string-append "Node '" nid "' not found"))))
      node-ids)
    (let* ([slug (let ([sorted (list-sort string<? (list-copy node-ids))])
                   (apply string-append
                     (let loop ([ids sorted] [acc '()])
                       (if (null? ids)
                           (reverse acc)
                           (loop (cdr ids)
                                 (if (null? acc)
                                     (list (car ids))
                                     (cons (car ids) (cons "-" acc))))))))]
           [nogood-id (string-append "nogood-" slug)]
           [nogood-id (let loop ([nid nogood-id] [suffix 2])
                        (if (exists (lambda (ng) (string=? (nogood-id ng) nid))
                                    (network-nogoods net))
                            (loop (string-append "nogood-" slug "-"
                                                  (number->string suffix))
                                  (+ suffix 1))
                            nid))]
           [ng (make-nogood nogood-id (list-copy node-ids)
                            (current-iso8601) "")]
           [_ (network-nogoods-set! net
                (append (network-nogoods net) (list ng)))]
           [_ (log-event! net "nogood" nogood-id
                (let ([p (open-output-string)])
                  (display node-ids p)
                  (get-output-string p)))])
      ;; Check if contradiction is active
      (let ([all-in (for-all
                      (lambda (nid)
                        (string=? (node-truth-value
                                    (hashtable-ref (network-nodes net) nid #f))
                                  "IN"))
                      node-ids)])
        (if (not all-in)
            '()
            (let ([culprits (network-find-culprits net node-ids)])
              (let ([victim-id
                      (if (not (null? culprits))
                          (begin
                            (log-event! net "backtrack"
                              (alist-ref "premise" (car culprits) "")
                              (string-append "culprit for " nogood-id))
                            (alist-ref "premise" (car culprits) ""))
                          (let ([candidates
                                  (list-sort
                                    (lambda (a b)
                                      (< (cdr a) (cdr b)))
                                    (map (lambda (nid)
                                           (cons nid
                                             (length (string-set->list
                                                       (node-dependents
                                                         (hashtable-ref
                                                           (network-nodes net) nid #f))))))
                                         node-ids))])
                            (caar candidates)))])
                (network-retract! net victim-id)))))))

  (define (network-get-belief-set net)
    (let ([result '()])
      (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
        (vector-for-each
          (lambda (k v)
            (when (string=? (node-truth-value v) "IN")
              (set! result (cons k result))))
          keys vals))
      (reverse result)))

  (define (network-recompute-all! net)
    (let ([all-changed (string-set-empty)]
          [max-iterations (+ (hashtable-size (network-nodes net)) 1)])
      (let loop ([iter 0])
        (when (< iter max-iterations)
          (let ([changed-this-pass '()])
            (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
              (vector-for-each
                (lambda (nid node)
                  (when (and (not (null? (node-justifications node)))
                             (not (alist-ref "_retracted" (node-metadata node) #f)))
                    (let ([old (node-truth-value node)]
                          [new (compute-truth net node)])
                      (unless (string=? old new)
                        (node-truth-value-set! node new)
                        (set! changed-this-pass (cons nid changed-this-pass))
                        (log-event! net "recompute" nid new)))))
                keys vals))
            (unless (null? changed-this-pass)
              (for-each (lambda (nid) (string-set-add! all-changed nid))
                        changed-this-pass)
              (loop (+ iter 1))))))
      (string-set->list all-changed)))

  (define (network-add-justification! net node-id justification)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (unless node
        (error 'network-add-justification!
          (string-append "Node '" node-id "' not found")))
      (let ([old-value (node-truth-value node)])
        ;; Register in dependents
        (for-each
          (lambda (ant-id)
            (let ([ant (hashtable-ref (network-nodes net) ant-id #f)])
              (when ant (string-set-add! (node-dependents ant) node-id))))
          (justification-antecedents justification))
        (for-each
          (lambda (out-id)
            (let ([out (hashtable-ref (network-nodes net) out-id #f)])
              (when out (string-set-add! (node-dependents out) node-id))))
          (justification-outlist justification))
        ;; Add justification
        (node-justifications-set! node
          (append (node-justifications node) (list justification)))
        ;; Compute new truth
        (let ([new-value (compute-truth net node)]
              [changed '()])
          (unless (string=? old-value new-value)
            (node-truth-value-set! node new-value)
            (set! changed (cons node-id (propagate! net node-id))))
          (log-event! net "add-justification" node-id new-value)
          (list (cons "node_id" node-id)
                (cons "old_truth_value" old-value)
                (cons "new_truth_value" new-value)
                (cons "changed" changed))))))

  (define (network-challenge! net target-id reason . opts)
    (let ([challenge-id (if (null? opts) #f (car opts))])
      (let ([target (hashtable-ref (network-nodes net) target-id #f)])
        (unless target
          (error 'network-challenge!
            (string-append "Node '" target-id "' not found")))
        ;; Generate challenge ID
        (let ([challenge-id
                (or challenge-id
                    (let loop ([cid (string-append "challenge-" target-id)]
                               [suffix 1])
                      (if (hashtable-ref (network-nodes net) cid #f)
                          (loop (string-append "challenge-" target-id "-"
                                               (number->string (+ suffix 1)))
                                (+ suffix 1))
                          cid)))])
          (when (hashtable-ref (network-nodes net) challenge-id #f)
            (error 'network-challenge!
              (string-append "Challenge node '" challenge-id "' already exists")))
          ;; Create challenge node (premise — IN by default)
          (let ([challenge-node
                  (network-add-node! net challenge-id reason
                    (list (cons 'metadata
                            (list (cons "challenge_target" target-id)))))])
            ;; Add challenge to target's outlist
            (if (null? (node-justifications target))
                ;; Target is a premise — convert to justified
                (node-justifications-set! target
                  (list (make-justification "SL" '() (list challenge-id) "" "")))
                ;; Add to all existing justifications
                (for-each
                  (lambda (j)
                    ;; Since outlist is immutable in the record, we need to
                    ;; reconstruct. Actually, let me handle this differently.
                    ;; We'll need mutable outlist... Let me work around this.
                    (void))
                  (node-justifications target)))
            ;; Since justification fields are immutable, rebuild justifications
            ;; with the challenge added to outlists
            (let ([new-justs
                    (if (null? (node-justifications target))
                        (list (make-justification "SL" '() (list challenge-id) "" ""))
                        (map (lambda (j)
                               (make-justification
                                 (justification-type j)
                                 (justification-antecedents j)
                                 (append (justification-outlist j) (list challenge-id))
                                 (justification-label j)
                                 (justification-content-hash j)))
                             (node-justifications target)))])
              ;; Check if target was premise with no justifications before
              (when (null? (node-justifications target))
                (set! new-justs (list (make-justification "SL" '() (list challenge-id) "" ""))))
              (node-justifications-set! target new-justs))
            ;; Register challenge as affecting target
            (string-set-add! (node-dependents challenge-node) target-id)
            ;; Track challenge on target metadata
            (let ([challenges (alist-ref "challenges" (node-metadata target) '())])
              (node-metadata-set! target
                (alist-set "challenges"
                  (append challenges (list challenge-id))
                  (node-metadata target))))
            ;; Recompute target
            (let ([old-value (node-truth-value target)]
                  [new-value (compute-truth net target)]
                  [changed '()])
              (if (not (string=? old-value new-value))
                  (begin
                    (node-truth-value-set! target new-value)
                    (set! changed (cons target-id (propagate! net target-id)))
                    (log-event! net "challenge" target-id new-value))
                  (log-event! net "challenge" target-id
                    (string-append "unchanged (" old-value ")")))
              (list (cons "challenge_id" challenge-id)
                    (cons "target_id" target-id)
                    (cons "changed" changed))))))))

  (define (network-defend! net target-id challenge-id reason . opts)
    (let ([defense-id (if (null? opts) #f (car opts))])
      (let ([target (hashtable-ref (network-nodes net) target-id #f)])
        (unless target
          (error 'network-defend!
            (string-append "Node '" target-id "' not found")))
        (unless (hashtable-ref (network-nodes net) challenge-id #f)
          (error 'network-defend!
            (string-append "Challenge '" challenge-id "' not found")))
        (let ([defense-id
                (or defense-id
                    (let loop ([did (string-append "defense-" challenge-id)]
                               [suffix 1])
                      (if (hashtable-ref (network-nodes net) did #f)
                          (loop (string-append "defense-" challenge-id "-"
                                               (number->string (+ suffix 1)))
                                (+ suffix 1))
                          did)))])
          (when (hashtable-ref (network-nodes net) defense-id #f)
            (error 'network-defend!
              (string-append "Defense node '" defense-id "' already exists")))
          ;; Defense challenges the challenge — same mechanism
          (let ([result (network-challenge! net challenge-id reason defense-id)])
            ;; Update metadata
            (let ([defense-node (hashtable-ref (network-nodes net) defense-id #f)])
              (node-metadata-set! defense-node
                (alist-set "defense_target" challenge-id
                  (alist-set "defends" target-id
                    (node-metadata defense-node)))))
            (list (cons "defense_id" defense-id)
                  (cons "challenge_id" challenge-id)
                  (cons "target_id" target-id)
                  (cons "changed" (alist-ref "changed" result '()))))))))

  (define (network-supersede! net old-id new-id)
    (let ([old-node (hashtable-ref (network-nodes net) old-id #f)]
          [new-node (hashtable-ref (network-nodes net) new-id #f)])
      (unless old-node
        (error 'network-supersede! (string-append "Node '" old-id "' not found")))
      (unless new-node
        (error 'network-supersede! (string-append "Node '" new-id "' not found")))
      ;; Add new-id to old-node's outlist
      (if (null? (node-justifications old-node))
          ;; Old is a premise — convert to justified with outlist
          (node-justifications-set! old-node
            (list (make-justification "SL" '() (list new-id) "" "")))
          ;; Add new-id to outlist of all justifications
          (node-justifications-set! old-node
            (map (lambda (j)
                   (if (member new-id (justification-outlist j))
                       j
                       (make-justification
                         (justification-type j)
                         (justification-antecedents j)
                         (append (justification-outlist j) (list new-id))
                         (justification-label j)
                         (justification-content-hash j))))
                 (node-justifications old-node))))
      ;; Register
      (string-set-add! (node-dependents new-node) old-id)
      ;; Metadata
      (node-metadata-set! old-node
        (alist-set "superseded_by" new-id (node-metadata old-node)))
      (let ([supersedes (alist-ref "supersedes" (node-metadata new-node) '())])
        (unless (member old-id supersedes)
          (node-metadata-set! new-node
            (alist-set "supersedes"
              (append supersedes (list old-id))
              (node-metadata new-node)))))
      ;; Recompute and propagate
      (let ([old-value (node-truth-value old-node)]
            [new-value (compute-truth net old-node)]
            [changed '()])
        (if (not (string=? old-value new-value))
            (begin
              (node-truth-value-set! old-node new-value)
              (set! changed (cons old-id (propagate! net old-id)))
              (log-event! net "supersede" old-id
                (string-append "superseded by " new-id)))
            (log-event! net "supersede" old-id
              (string-append "superseded by " new-id " (unchanged)")))
        (list (cons "old_id" old-id)
              (cons "new_id" new-id)
              (cons "changed" changed)))))

  (define (network-convert-to-premise! net node-id)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (unless node
        (error 'network-convert-to-premise!
          (string-append "Node '" node-id "' not found")))
      (let ([old-count (length (node-justifications node))])
        ;; Remove from dependents of antecedents/outlist
        (for-each
          (lambda (j)
            (for-each
              (lambda (ant-id)
                (let ([ant (hashtable-ref (network-nodes net) ant-id #f)])
                  (when ant (string-set-remove! (node-dependents ant) node-id))))
              (justification-antecedents j))
            (for-each
              (lambda (out-id)
                (let ([out (hashtable-ref (network-nodes net) out-id #f)])
                  (when out (string-set-remove! (node-dependents out) node-id))))
              (justification-outlist j)))
          (node-justifications node))
        (node-justifications-set! node '())
        (node-supporting-justification-set! node #f)
        ;; Premise is IN by default
        (let ([changed '()])
          (if (not (string=? (node-truth-value node) "IN"))
              (begin
                (node-truth-value-set! node "IN")
                (set! changed (cons node-id (propagate! net node-id)))
                (log-event! net "convert-to-premise" node-id "IN"))
              (log-event! net "convert-to-premise" node-id "IN (unchanged)"))
          (list (cons "node_id" node-id)
                (cons "old_justifications" old-count)
                (cons "truth_value" (node-truth-value node))
                (cons "changed" changed))))))

  (define (network-remove-justification! net node-id index)
    (let ([node (hashtable-ref (network-nodes net) node-id #f)])
      (unless node
        (error 'network-remove-justification!
          (string-append "Node '" node-id "' not found")))
      (when (null? (node-justifications node))
        (error 'network-remove-justification!
          (string-append "Node '" node-id "' is a premise (no justifications)")))
      (when (or (< index 0) (>= index (length (node-justifications node))))
        (error 'network-remove-justification!
          (string-append "Justification index out of range")))
      (when (= (length (node-justifications node)) 1)
        (error 'network-remove-justification!
          (string-append "Node '" node-id
            "' has only one justification; use convert-to-premise or retract")))
      (let* ([old-value (node-truth-value node)]
             [removed (list-ref (node-justifications node) index)]
             [new-justs (append (list-head (node-justifications node) index)
                               (list-tail (node-justifications node) (+ index 1)))])
        (node-justifications-set! node new-justs)
        ;; Clean up dependents for refs no longer in any justification
        (let ([remaining-refs (string-set-empty)])
          (for-each
            (lambda (j)
              (for-each (lambda (a) (string-set-add! remaining-refs a))
                        (justification-antecedents j))
              (for-each (lambda (o) (string-set-add! remaining-refs o))
                        (justification-outlist j)))
            new-justs)
          (for-each
            (lambda (ant-id)
              (unless (string-set-contains? remaining-refs ant-id)
                (let ([ant (hashtable-ref (network-nodes net) ant-id #f)])
                  (when ant (string-set-remove! (node-dependents ant) node-id)))))
            (justification-antecedents removed))
          (for-each
            (lambda (out-id)
              (unless (string-set-contains? remaining-refs out-id)
                (let ([out (hashtable-ref (network-nodes net) out-id #f)])
                  (when out (string-set-remove! (node-dependents out) node-id)))))
            (justification-outlist removed)))
        (let ([new-value (compute-truth net node)]
              [changed '()])
          (unless (string=? old-value new-value)
            (node-truth-value-set! node new-value)
            (set! changed (cons node-id (propagate! net node-id))))
          (log-event! net "remove-justification" node-id new-value)
          (list (cons "node_id" node-id)
                (cons "old_truth_value" old-value)
                (cons "new_truth_value" new-value)
                (cons "removed" (list (cons "type" (justification-type removed))
                                      (cons "antecedents" (justification-antecedents removed))
                                      (cons "outlist" (justification-outlist removed))
                                      (cons "label" (justification-label removed))))
                (cons "remaining" (length new-justs))
                (cons "changed" changed))))))

  (define (network-summarize! net summary-id text over . opts)
    (let ([source (if (null? opts) "" (car opts))])
      (for-each
        (lambda (nid)
          (unless (hashtable-ref (network-nodes net) nid #f)
            (error 'network-summarize!
              (string-append "Node '" nid "' not found"))))
        over)
      (when (hashtable-ref (network-nodes net) summary-id #f)
        (error 'network-summarize!
          (string-append "Node '" summary-id "' already exists")))
      (let ([node (network-add-node! net summary-id text
                    (list (cons 'justifications
                            (list (make-justification "SL" (list-copy over) '() "summarizes" "")))
                          (cons 'source source)
                          (cons 'metadata (list (cons "summarizes" (list-copy over))))))])
        ;; Mark summarized nodes
        (for-each
          (lambda (nid)
            (let ([n (hashtable-ref (network-nodes net) nid #f)])
              (let ([covered (alist-ref "summarized_by" (node-metadata n) '())])
                (unless (member summary-id covered)
                  (node-metadata-set! n
                    (alist-set "summarized_by"
                      (append covered (list summary-id))
                      (node-metadata n)))))))
          over)
        (list (cons "summary_id" summary-id)
              (cons "over" (list-copy over))
              (cons "truth_value" (node-truth-value node))))))

) ;; end library
