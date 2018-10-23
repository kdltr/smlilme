(module live (define)
(import (rename scheme (define define0)) (chicken syntax) (chicken memory representation))
(import-for-syntax matchable (chicken base) (chicken memory representation))

(begin-for-syntax
  (define (procedure-sym? s)
    (and-let* ((sym (strip-syntax s))
               (p (assq sym (##sys#current-environment)))
               (qsym (cdr p)))
         (procedure? (block-ref qsym 0)))))

(define-syntax define
  (ir-macro-transformer
    (lambda (sexp inject compare)
      (match sexp
        ((_ ((and name (? procedure-sym?)) . args) . body)
         `(mutate-procedure! ,name (lambda (old) (lambda ,args ,@body))))
        ((_ (and name (? procedure-sym?)) ((? (cut compare 'lambda <>)) args . body))
         `(mutate-procedure! ,name (lambda (old) (lambda ,args ,@body))))
        (else
          `(define0 ,@(cdr sexp))))
         )))
)

(import live)


;; (safe code…) macro

(define dirty #f)

(define-syntax safe
  (syntax-rules ()
    ((safe body)
     (handle-exceptions exn
       (begin
         (set! dirty #t)
         (signal exn))
       body
       (set! dirty #f)))))


;; Live reloading pseudo filesystem

(define *inotify-flags*
  '(create close-write delete delete-self))

(define (load-binary-file filename)
  (let* ((size (file-size filename)) (buf (make-u8vector size 0)))
    (with-input-from-file
      filename
      (lambda () (read-u8vector! size buf) (u8vector->blob/shared buf)))))

(define *file-readers*
  `((".*\\.scm" . ,(lambda (file) (with-input-from-file file read)))
    (".*\\.glsl" . ,(lambda (file) (with-input-from-file file read-string)))
    (".*\\.prog" . ,(lambda (file) (with-input-from-file file read-list)))
    (".*" . ,load-binary-file)))

(define (find-reader filename)
  (let lp ((readers *file-readers*))
    (if (irregex-match (caar readers) filename)
        (cdar readers)
        (lp (cdr readers)))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1)
            (symbol->string s2)))

(define (pathname->list path)
  (let* ((base-origin base-directory elements (decompose-directory (pathname-strip-extension path)))
         (syms (map string->symbol elements)))
    (if (eq? '|.| (car syms)) (cdr syms) syms)))

(define (load-file file pseudo-path)
  (fs:write pseudo-path ((find-reader file) file)))

(define (mount-pseudofs dir filename pseudo-path)
  (let* ((path (make-pathname dir filename))
         (symfile (string->symbol (pathname-strip-extension filename)))
         (new-pseudo-path (append pseudo-path (list symfile))))
    (ino:add-watch! path *inotify-flags*)
    (if (directory? path)
        (begin
          (fs:mkdir new-pseudo-path)
          (for-each
            (lambda (new-filename)
              (mount-pseudofs path new-filename new-pseudo-path))
            (directory path)))
        (load-file path new-pseudo-path))))

(define (propagate-fs-changes ev)
  (let ((flags (sort (ino:event-flags ev) symbol<?))
        (pathname (ino:event->pathname ev)))
    (cond ((equal? '(create) flags)
           (ino:add-watch! pathname *inotify-flags*)
           (load-file pathname (pathname->list pathname)))
          ((equal? '(create isdir) flags)
           (mount-pseudofs (ino:wd->path (ino:event-wd ev)) (ino:event-name ev)
                           (butlast (pathname->list pathname))))
          ((equal? '(close-write) flags)
           (load-file pathname (pathname->list pathname))))))

(define *ino-thread* (thread-start! (lambda () '())))

(define (watch-for-fs-changes)
  (let ((maybe-events (thread-join! *ino-thread* 0 #f)))
    (when maybe-events
      (print "inotify events: " maybe-events)
      (for-each propagate-fs-changes maybe-events)
      (set! *ino-thread* (thread-start! ino:next-events!)))))
