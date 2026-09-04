(library (ftl reasons json)
  (export json-read json-write)
  (import (chezscheme))

  ;; Minimal JSON parser/serializer for SQLite column values.
  ;; Handles: strings, numbers, booleans, null, arrays, objects.
  ;; Objects become alists. Arrays become lists.

  ;; ---- Reader ----

  (define (json-read str)
    (let ([port (open-input-string str)])
      (let ([result (read-value port)])
        (close-input-port port)
        result)))

  (define (skip-ws port)
    (let loop ()
      (let ([c (peek-char port)])
        (when (and (not (eof-object? c)) (char-whitespace? c))
          (read-char port)
          (loop)))))

  (define (read-value port)
    (skip-ws port)
    (let ([c (peek-char port)])
      (cond
        [(eof-object? c) (error 'json-read "unexpected end of input")]
        [(char=? c #\") (read-json-string port)]
        [(char=? c #\[) (read-json-array port)]
        [(char=? c #\{) (read-json-object port)]
        [(char=? c #\t) (read-literal port "true" #t)]
        [(char=? c #\f) (read-literal port "false" #f)]
        [(char=? c #\n) (read-literal port "null" 'null)]
        [(or (char=? c #\-) (char-numeric? c)) (read-json-number port)]
        [else (error 'json-read (format "unexpected character: ~a" c))])))

  (define (read-json-string port)
    (read-char port)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([c (read-char port)])
          (cond
            [(eof-object? c) (error 'json-read "unterminated string")]
            [(char=? c #\") (get-output-string out)]
            [(char=? c #\\)
             (let ([esc (read-char port)])
               (cond
                 [(char=? esc #\") (write-char #\" out)]
                 [(char=? esc #\\) (write-char #\\ out)]
                 [(char=? esc #\/) (write-char #\/ out)]
                 [(char=? esc #\b) (write-char #\backspace out)]
                 [(char=? esc #\f) (write-char #\page out)]
                 [(char=? esc #\n) (write-char #\newline out)]
                 [(char=? esc #\r) (write-char #\return out)]
                 [(char=? esc #\t) (write-char #\tab out)]
                 [(char=? esc #\u)
                  (let ([hex (read-hex4 port)])
                    (write-char (integer->char hex) out))]
                 [else (write-char esc out)])
               (loop))]
            [else (write-char c out) (loop)])))))

  (define (read-hex4 port)
    (let ([s (make-string 4)])
      (do ([i 0 (+ i 1)])
          ((= i 4))
        (string-set! s i (read-char port)))
      (string->number s 16)))

  (define (read-json-array port)
    (read-char port)
    (skip-ws port)
    (if (char=? (peek-char port) #\])
        (begin (read-char port) '())
        (let loop ([acc '()])
          (let ([val (read-value port)])
            (skip-ws port)
            (let ([c (read-char port)])
              (cond
                [(char=? c #\]) (reverse (cons val acc))]
                [(char=? c #\,) (loop (cons val acc))]
                [else (error 'json-read "expected , or ]")]))))))

  (define (read-json-object port)
    (read-char port)
    (skip-ws port)
    (if (char=? (peek-char port) #\})
        (begin (read-char port) '())
        (let loop ([acc '()])
          (skip-ws port)
          (let ([key (read-json-string port)])
            (skip-ws port)
            (read-char port) ;; colon
            (let ([val (read-value port)])
              (skip-ws port)
              (let ([c (read-char port)])
                (cond
                  [(char=? c #\}) (reverse (cons (cons key val) acc))]
                  [(char=? c #\,) (loop (cons (cons key val) acc))]
                  [else (error 'json-read "expected , or }")]))))))  )

  (define (read-literal port expected value)
    (let ([len (string-length expected)])
      (do ([i 0 (+ i 1)])
          ((= i len))
        (read-char port))
      value))

  (define (read-json-number port)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([c (peek-char port)])
          (when (and (not (eof-object? c))
                     (or (char-numeric? c)
                         (char=? c #\-)
                         (char=? c #\+)
                         (char=? c #\.)
                         (char=? c #\e)
                         (char=? c #\E)))
            (write-char (read-char port) out)
            (loop))))
      (let ([s (get-output-string out)])
        (or (string->number s) (error 'json-read (format "bad number: ~a" s))))))

  ;; ---- Writer ----

  (define (json-write val)
    (let ([port (open-output-string)])
      (write-value val port)
      (get-output-string port)))

  (define (write-value val port)
    (cond
      [(string? val) (write-json-string val port)]
      [(number? val) (write-json-number val port)]
      [(boolean? val) (display (if val "true" "false") port)]
      [(eq? val 'null) (display "null" port)]
      [(null? val) (display "[]" port)]
      [(and (pair? val) (pair? (car val)) (string? (caar val)))
       (write-json-object val port)]
      [(list? val) (write-json-array val port)]
      [(symbol? val) (write-json-string (symbol->string val) port)]
      [else (error 'json-write (format "cannot serialize: ~s" val))]))

  (define (write-json-string s port)
    (write-char #\" port)
    (string-for-each
      (lambda (c)
        (cond
          [(char=? c #\") (display "\\\"" port)]
          [(char=? c #\\) (display "\\\\" port)]
          [(char=? c #\newline) (display "\\n" port)]
          [(char=? c #\return) (display "\\r" port)]
          [(char=? c #\tab) (display "\\t" port)]
          [(char<? c #\space)
           (display (format "\\u~4,'0x" (char->integer c)) port)]
          [else (write-char c port)]))
      s)
    (write-char #\" port))

  (define (write-json-number n port)
    (if (and (exact? n) (integer? n))
        (display n port)
        (display (exact->inexact n) port)))

  (define (write-json-array lst port)
    (write-char #\[ port)
    (let loop ([items lst] [first #t])
      (unless (null? items)
        (unless first (write-char #\, port))
        (write-value (car items) port)
        (loop (cdr items) #f)))
    (write-char #\] port))

  (define (write-json-object alist port)
    (write-char #\{ port)
    (let loop ([pairs alist] [first #t])
      (unless (null? pairs)
        (unless first (write-char #\, port))
        (let ([pair (car pairs)])
          (write-json-string (if (symbol? (car pair))
                                 (symbol->string (car pair))
                                 (car pair))
                             port)
          (write-char #\: port)
          (write-value (cdr pair) port))
        (loop (cdr pairs) #f)))
    (write-char #\} port))

) ;; end library
