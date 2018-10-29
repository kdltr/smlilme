(module live-define (define)
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
          `(define0 ,@(cdr sexp))))))))
