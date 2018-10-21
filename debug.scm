(cond-expand
  (debug (import (chicken time posix)
                 (chicken format)
                 (chicken module))
         (define (debug msg . args)
           (fprintf (current-error-port) "[~a] ~a ~?~%"
                    (seconds->string)
                    (or (current-module) 'Toplevel)
                    msg args)))
  (else (define debug void)))
