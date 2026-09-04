(library (ftl reasons sqlite)
  (export
    sqlite-open sqlite-close
    sqlite-exec sqlite-exec/callback
    sqlite-prepare sqlite-finalize sqlite-reset
    sqlite-step sqlite-step-row
    sqlite-bind-text sqlite-bind-int sqlite-bind-null
    sqlite-column-text sqlite-column-int sqlite-column-count
    sqlite-column-name
    sqlite-query sqlite-execute
    SQLITE-OK SQLITE-ROW SQLITE-DONE)
  (import (chezscheme))

  (define lib
    (load-shared-object
      (case (machine-type)
        [(arm64osx a6osx ta6osx tarm64osx) "libsqlite3.dylib"]
        [else "libsqlite3.so"])))

  (define SQLITE-OK 0)
  (define SQLITE-ROW 100)
  (define SQLITE-DONE 101)

  ;; ---- Low-level FFI ----

  (define sqlite3-open-ffi
    (foreign-procedure "sqlite3_open" (string void*) int))

  (define sqlite3-close-ffi
    (foreign-procedure "sqlite3_close" (void*) int))

  (define sqlite3-exec-ffi
    (foreign-procedure "sqlite3_exec" (void* string void* void* void*) int))

  (define sqlite3-errmsg-ffi
    (foreign-procedure "sqlite3_errmsg" (void*) string))

  (define sqlite3-prepare-v2-ffi
    (foreign-procedure "sqlite3_prepare_v2" (void* string int void* void*) int))

  (define sqlite3-step-ffi
    (foreign-procedure "sqlite3_step" (void*) int))

  (define sqlite3-finalize-ffi
    (foreign-procedure "sqlite3_finalize" (void*) int))

  (define sqlite3-reset-ffi
    (foreign-procedure "sqlite3_reset" (void*) int))

  (define sqlite3-bind-text-ffi
    (foreign-procedure "sqlite3_bind_text" (void* int string int void*) int))

  (define sqlite3-bind-int-ffi
    (foreign-procedure "sqlite3_bind_int" (void* int int) int))

  (define sqlite3-bind-null-ffi
    (foreign-procedure "sqlite3_bind_null" (void* int) int))

  (define sqlite3-column-text-ffi
    (foreign-procedure "sqlite3_column_text" (void* int) string))

  (define sqlite3-column-int-ffi
    (foreign-procedure "sqlite3_column_int" (void* int) int))

  (define sqlite3-column-count-ffi
    (foreign-procedure "sqlite3_column_count" (void*) int))

  (define sqlite3-column-name-ffi
    (foreign-procedure "sqlite3_column_name" (void* int) string))

  ;; SQLITE_TRANSIENT = -1 as a pointer value
  (define SQLITE-TRANSIENT
    (let ([p (foreign-alloc (foreign-sizeof 'void*))])
      (foreign-set! 'iptr p 0 -1)
      (foreign-ref 'void* p 0)))

  ;; ---- High-level wrappers ----

  (define (sqlite-open filename)
    (let ([ppdb (foreign-alloc (foreign-sizeof 'void*))])
      (let ([rc (sqlite3-open-ffi filename ppdb)])
        (if (= rc SQLITE-OK)
            (let ([db (foreign-ref 'void* ppdb 0)])
              (foreign-free ppdb)
              db)
            (begin
              (foreign-free ppdb)
              (error 'sqlite-open
                (format "failed to open ~a (code ~a)" filename rc)))))))

  (define (sqlite-close db)
    (sqlite3-close-ffi db))

  (define (sqlite-exec db sql)
    (let ([perr (foreign-alloc (foreign-sizeof 'void*))])
      (foreign-set! 'void* perr 0 0)
      (let ([rc (sqlite3-exec-ffi db sql 0 0 perr)])
        (let ([err (foreign-ref 'void* perr 0)])
          (foreign-free perr)
          (unless (= rc SQLITE-OK)
            (error 'sqlite-exec
              (format "SQL error (~a): ~a" rc (sqlite3-errmsg-ffi db))))))))

  (define (sqlite-exec/callback db sql callback)
    (error 'sqlite-exec/callback "not implemented — use sqlite-query instead"))

  (define (sqlite-prepare db sql)
    (let ([pstmt (foreign-alloc (foreign-sizeof 'void*))]
          [ptail (foreign-alloc (foreign-sizeof 'void*))])
      (let ([rc (sqlite3-prepare-v2-ffi db sql -1 pstmt ptail)])
        (let ([stmt (foreign-ref 'void* pstmt 0)])
          (foreign-free pstmt)
          (foreign-free ptail)
          (unless (= rc SQLITE-OK)
            (error 'sqlite-prepare
              (format "prepare failed (~a): ~a" rc (sqlite3-errmsg-ffi db))))
          stmt))))

  (define (sqlite-finalize stmt)
    (sqlite3-finalize-ffi stmt))

  (define (sqlite-reset stmt)
    (sqlite3-reset-ffi stmt))

  (define (sqlite-step stmt)
    (sqlite3-step-ffi stmt))

  (define (sqlite-step-row stmt)
    (let ([rc (sqlite3-step-ffi stmt)])
      (cond
        [(= rc SQLITE-ROW) #t]
        [(= rc SQLITE-DONE) #f]
        [else (error 'sqlite-step-row (format "step error: ~a" rc))])))

  (define (sqlite-bind-text stmt index value)
    (sqlite3-bind-text-ffi stmt index value -1 SQLITE-TRANSIENT))

  (define (sqlite-bind-int stmt index value)
    (sqlite3-bind-int-ffi stmt index value))

  (define (sqlite-bind-null stmt index)
    (sqlite3-bind-null-ffi stmt index))

  (define (sqlite-column-text stmt index)
    (let ([v (sqlite3-column-text-ffi stmt index)])
      (or v "")))

  (define (sqlite-column-int stmt index)
    (sqlite3-column-int-ffi stmt index))

  (define (sqlite-column-count stmt)
    (sqlite3-column-count-ffi stmt))

  (define (sqlite-column-name stmt index)
    (sqlite3-column-name-ffi stmt index))

  ;; ---- Convenience ----

  (define (sqlite-query db sql . bindings)
    (let ([stmt (sqlite-prepare db sql)]
          [params (if (null? bindings) '() (car bindings))])
      (let loop ([i 1] [ps params])
        (unless (null? ps)
          (let ([v (car ps)])
            (cond
              [(string? v) (sqlite-bind-text stmt i v)]
              [(integer? v) (sqlite-bind-int stmt i v)]
              [(not v) (sqlite-bind-null stmt i)]
              [else (sqlite-bind-text stmt i (format "~a" v))]))
          (loop (+ i 1) (cdr ps))))
      (let ([ncols (sqlite-column-count stmt)]
            [rows '()])
        (let loop ()
          (when (sqlite-step-row stmt)
            (let ([row (let col-loop ([i 0] [acc '()])
                         (if (= i ncols)
                             (reverse acc)
                             (col-loop (+ i 1)
                                       (cons (sqlite-column-text stmt i) acc))))])
              (set! rows (cons row rows))
              (loop))))
        (sqlite-finalize stmt)
        (reverse rows))))

  (define (sqlite-execute db sql . bindings)
    (let ([stmt (sqlite-prepare db sql)]
          [params (if (null? bindings) '() (car bindings))])
      (let loop ([i 1] [ps params])
        (unless (null? ps)
          (let ([v (car ps)])
            (cond
              [(string? v) (sqlite-bind-text stmt i v)]
              [(integer? v) (sqlite-bind-int stmt i v)]
              [(not v) (sqlite-bind-null stmt i)]
              [else (sqlite-bind-text stmt i (format "~a" v))]))
          (loop (+ i 1) (cdr ps))))
      (sqlite-step stmt)
      (sqlite-finalize stmt)))

) ;; end library
