(library (ftl reasons merkle)
  (export compute-text-hash compute-content-hash compute-merkle-hash
          verify-node verify-justification verify-all backfill-hashes)
  (import (chezscheme) (ftl reasons))

  ;; Pure Scheme SHA-256 implementation

  (define K
    '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
       #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
       #xd807aa98 #x12835b01 #x243185be #x550c7dc3
       #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
       #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
       #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
       #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
       #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
       #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
       #x650a7354 #x766a0abb #x81c2c92e #x92722c85
       #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
       #xd192e819 #xd6990624 #xf40e3585 #x106aa070
       #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
       #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
       #x748f82ee #x78a5636f #x84c87814 #x8cc70208
       #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

  (define (u32 x) (bitwise-and x #xffffffff))
  (define (u32+ . args) (u32 (apply + args)))
  (define (rotr x n) (u32 (bitwise-ior (bitwise-arithmetic-shift-right x n)
                                        (bitwise-arithmetic-shift-left x (- 32 n)))))
  (define (shr x n) (bitwise-arithmetic-shift-right x n))
  (define (ch x y z) (u32 (bitwise-xor (bitwise-and x y) (bitwise-and (bitwise-not x) z))))
  (define (maj x y z) (u32 (bitwise-xor (bitwise-and x y) (bitwise-and x z) (bitwise-and y z))))
  (define (bsig0 x) (u32 (bitwise-xor (rotr x 2) (rotr x 13) (rotr x 22))))
  (define (bsig1 x) (u32 (bitwise-xor (rotr x 6) (rotr x 11) (rotr x 25))))
  (define (ssig0 x) (u32 (bitwise-xor (rotr x 7) (rotr x 18) (shr x 3))))
  (define (ssig1 x) (u32 (bitwise-xor (rotr x 17) (rotr x 19) (shr x 10))))

  (define (sha256-pad msg)
    (let* ([len (bytevector-length msg)]
           [bit-len (* len 8)]
           [pad-len (let ([r (mod (+ len 1) 64)])
                      (if (<= r 56) (- 56 r) (- 120 r)))]
           [total (+ len 1 pad-len 8)]
           [out (make-bytevector total 0)])
      (bytevector-copy! msg 0 out 0 len)
      (bytevector-u8-set! out len #x80)
      (do ([i 0 (+ i 1)])
          ((= i 8))
        (bytevector-u8-set! out (- total 1 i)
          (bitwise-and (bitwise-arithmetic-shift-right bit-len (* i 8)) #xff)))
      out))

  (define (sha256-bytes bv)
    (let ([padded (sha256-pad bv)]
          [h0 #x6a09e667] [h1 #xbb67ae85] [h2 #x3c6ef372] [h3 #xa54ff53a]
          [h4 #x510e527f] [h5 #x9b05688c] [h6 #x1f83d9ab] [h7 #x5be0cd19])
      (let block-loop ([offset 0])
        (when (< offset (bytevector-length padded))
          (let ([w (make-vector 64 0)])
            (do ([t 0 (+ t 1)])
                ((= t 16))
              (vector-set! w t
                (bitwise-ior
                  (bitwise-arithmetic-shift-left (bytevector-u8-ref padded (+ offset (* t 4))) 24)
                  (bitwise-arithmetic-shift-left (bytevector-u8-ref padded (+ offset (* t 4) 1)) 16)
                  (bitwise-arithmetic-shift-left (bytevector-u8-ref padded (+ offset (* t 4) 2)) 8)
                  (bytevector-u8-ref padded (+ offset (* t 4) 3)))))
            (do ([t 16 (+ t 1)])
                ((= t 64))
              (vector-set! w t
                (u32+ (ssig1 (vector-ref w (- t 2)))
                      (vector-ref w (- t 7))
                      (ssig0 (vector-ref w (- t 15)))
                      (vector-ref w (- t 16)))))
            (let ([a h0] [b h1] [c h2] [d h3] [e h4] [f h5] [g h6] [hh h7])
              (do ([t 0 (+ t 1)])
                  ((= t 64)
                   (set! h0 (u32+ h0 a)) (set! h1 (u32+ h1 b))
                   (set! h2 (u32+ h2 c)) (set! h3 (u32+ h3 d))
                   (set! h4 (u32+ h4 e)) (set! h5 (u32+ h5 f))
                   (set! h6 (u32+ h6 g)) (set! h7 (u32+ h7 hh)))
                (let* ([t1 (u32+ hh (bsig1 e) (ch e f g) (vector-ref K t) (vector-ref w t))]
                       [t2 (u32+ (bsig0 a) (maj a b c))])
                  (set! hh g) (set! g f) (set! f e)
                  (set! e (u32+ d t1))
                  (set! d c) (set! c b) (set! b a)
                  (set! a (u32+ t1 t2))))))
          (block-loop (+ offset 64))))
      (let ([result (make-bytevector 32)])
        (do ([i 0 (+ i 1)]
             [vals (list h0 h1 h2 h3 h4 h5 h6 h7) (cdr vals)])
            ((= i 8))
          (let ([v (car vals)])
            (bytevector-u8-set! result (* i 4)     (bitwise-and (shr v 24) #xff))
            (bytevector-u8-set! result (+ (* i 4) 1) (bitwise-and (shr v 16) #xff))
            (bytevector-u8-set! result (+ (* i 4) 2) (bitwise-and (shr v 8) #xff))
            (bytevector-u8-set! result (+ (* i 4) 3) (bitwise-and v #xff))))
        result)))

  (define (sha256-hex str)
    (bytevector->hex-string (sha256-bytes (string->utf8 str))))

  (define hex-chars "0123456789abcdef")

  (define (bytevector->hex-string bv)
    (let* ([len (bytevector-length bv)]
           [s (make-string (* len 2))])
      (do ([i 0 (+ i 1)])
          ((= i len) s)
        (let ([b (bytevector-u8-ref bv i)])
          (string-set! s (* i 2) (string-ref hex-chars (bitwise-arithmetic-shift-right b 4)))
          (string-set! s (+ (* i 2) 1) (string-ref hex-chars (bitwise-and b #xf)))))))

  (define (compute-text-hash text)
    (sha256-hex text))

  (define (fresh-merkle-hash node-id net cache visiting)
    (cond
      [(hashtable-ref cache node-id #f) => (lambda (v) v)]
      [(hashtable-contains? visiting node-id)
       (compute-text-hash "")]
      [else
       (hashtable-set! visiting node-id #t)
       (let* ([node (hashtable-ref (network-nodes net) node-id #f)]
              [text-hash (compute-text-hash (node-text node))]
              [result
                (if (or (null? (node-justifications node))
                        (not (node-supporting-justification node)))
                    text-hash
                    (let* ([j (list-ref (node-justifications node)
                                        (node-supporting-justification node))]
                           [ch (fresh-content-hash text-hash j net cache visiting)])
                      (sha256-hex (string-append text-hash "|" ch))))])
         (hashtable-set! cache node-id result)
         result)]))

  (define (fresh-content-hash text-hash justification net cache visiting)
    (let ([ant-hashes
            (list-sort string<?
              (filter-map
                (lambda (a)
                  (and (hashtable-ref (network-nodes net) a #f)
                       (fresh-merkle-hash a net cache visiting)))
                (justification-antecedents justification)))])
      (let ([parts (if (null? ant-hashes)
                       text-hash
                       (string-append text-hash "|"
                         (apply string-append
                           (let loop ([hs ant-hashes] [acc '()])
                             (if (null? hs)
                                 (reverse acc)
                                 (loop (cdr hs)
                                       (if (null? acc)
                                           (list (car hs))
                                           (cons (car hs) (cons "|" acc)))))))))])
        (sha256-hex parts))))

  (define (filter-map f lst)
    (let loop ([l lst] [acc '()])
      (if (null? l)
          (reverse acc)
          (let ([v (f (car l))])
            (loop (cdr l) (if v (cons v acc) acc))))))

  (define (compute-merkle-hash node-id net)
    (fresh-merkle-hash node-id net
      (make-hashtable string-hash string=?)
      (make-hashtable string-hash string=?)))

  (define (compute-content-hash node justification net)
    (let ([text-hash (compute-text-hash (node-text node))])
      (fresh-content-hash text-hash justification net
        (make-hashtable string-hash string=?)
        (make-hashtable string-hash string=?))))

  (define (verify-node node)
    (if (or (not (node-text-hash node))
            (string=? (node-text-hash node) ""))
        #f
        (let ([current (compute-text-hash (node-text node))])
          (if (string=? current (node-text-hash node))
              #f
              (list (cons "type" "text_mutation")
                    (cons "node_id" (node-id node))
                    (cons "stored_hash" (node-text-hash node))
                    (cons "current_hash" current))))))

  (define (verify-justification node j-index net)
    (if (or (< j-index 0) (>= j-index (length (node-justifications node))))
        #f
        (let ([j (list-ref (node-justifications node) j-index)])
          (if (or (not (justification-content-hash j))
                  (string=? (justification-content-hash j) ""))
              #f
              (let ([current (compute-content-hash node j net)])
                (if (string=? current (justification-content-hash j))
                    #f
                    (list (cons "type" "chain_mutation")
                          (cons "node_id" (node-id node))
                          (cons "justification_index" j-index)
                          (cons "stored_hash" (justification-content-hash j))
                          (cons "current_hash" current))))))))

  (define (verify-all net)
    (let ([findings '()]
          [missing 0])
      (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
        (vector-for-each
          (lambda (nid node)
            (let ([f (verify-node node)])
              (if f
                  (set! findings (cons f findings))
                  (when (or (not (node-text-hash node))
                            (string=? (node-text-hash node) ""))
                    (set! missing (+ missing 1)))))
            (let loop ([i 0])
              (when (< i (length (node-justifications node)))
                (let ([f (verify-justification node i net)])
                  (if f
                      (set! findings (cons f findings))
                      (let ([j (list-ref (node-justifications node) i)])
                        (when (or (not (justification-content-hash j))
                                  (string=? (justification-content-hash j) ""))
                          (set! missing (+ missing 1))))))
                (loop (+ i 1)))))
          keys vals))
      (list (cons "findings" (reverse findings))
            (cons "missing_hashes" missing))))

  (define (backfill-hashes net)
    (let ([nodes-updated 0]
          [justifications-updated 0])
      (let-values ([(keys vals) (hashtable-entries (network-nodes net))])
        (vector-for-each
          (lambda (nid node)
            (when (or (not (node-text-hash node))
                      (string=? (node-text-hash node) ""))
              (node-text-hash-set! node (compute-text-hash (node-text node)))
              (set! nodes-updated (+ nodes-updated 1)))
            (for-each
              (lambda (j)
                (when (or (not (justification-content-hash j))
                          (string=? (justification-content-hash j) ""))
                  (justification-content-hash-set! j
                    (compute-content-hash node j net))
                  (set! justifications-updated (+ justifications-updated 1))))
              (node-justifications node)))
          keys vals))
      (list (cons "nodes_updated" nodes-updated)
            (cons "justifications_updated" justifications-updated))))

) ;; end library
