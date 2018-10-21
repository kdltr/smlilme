(import scran)

(define (unit-test name desc thunk)
  (print "Testing: " name " - " desc)
  (unless (thunk)
    (error "unit test failed" name)))


;; ********************************************************
;;
;; BEGIN TEST CODE
;;
;; Code in this section tests scran's basic functionality.
;;
;; ********************************************************

(unit-test "scran-ecs-basic"
           "Test whether basic entity creation works."
           (lambda ()
             (define height (component! (lambda (e h) h) "height"))
             (define growth-rate (component! (lambda (e g) g) "growth-rate"))
             (define growing-things
               (system! (list height growth-rate)
                        every: (lambda (e h g)
                                 (entity-component-set! e
                                                        height
                                                        (+ h g)))))
             (define pillar (entity! (list height 10)))
             (define tree (entity! (list height 10)
                                   (list growth-rate 1)))
             (system-execute growing-things)
             (let ((result (and (= (entity-component pillar height) 10)
                                (= (entity-component tree height) 11))))
               (!reset-scran!)
               result)))

(unit-test "scran-ecs-deletion"
           "Test whether removing component removes entity from appropriate system"
           (lambda ()
             (define height (component! (lambda (e h) h) "height"))
             (define growth-rate (component! (lambda (e g) g) "growth-rate"))
             (define growing-things
               (system! (list height growth-rate)
                        every: (lambda (e h g)
                                 (entity-component-set! e
                                                        height
                                                        (+ h g)))))
             (define pillar (entity! (list height 10)))
             (define tree (entity! (list height 10)
                                   (list growth-rate 1)))
             (system-execute growing-things)
             (let ((result (and (= (entity-component pillar height) 10)
                                (= (entity-component tree height) 11)
                                (begin
                                  ;; kill the tree, figuratively
                                  (remove-component! tree growth-rate)
                                  (system-execute growing-things)
                                  #t)
                                (= (entity-component pillar height) 10)
                                ;; the tree should not have grown.
                                (= (entity-component tree height) 11))))
               (!reset-scran!)
               result)))

(unit-test "scran-ecs-deletion-and-readdition"
           "Test whether removing and readding component removes and re-adds entity from/to appropriate system"
           (lambda ()
             (define height (component! (lambda (e h) h) "height"))
             (define growth-rate (component! (lambda (e g) g) "growth-rate"))
             (define growing-things
               (system! (list height growth-rate)
                        every: (lambda (e h g)
                                 (entity-component-set! e
                                                        height
                                                        (+ h g)))))
             (define pillar (entity! (list height 10)))
             (define tree (entity! (list height 10)
                                   (list growth-rate 1)))
             (system-execute growing-things)
             (let ((result (and (= (entity-component pillar height) 10)
                                (= (entity-component tree height) 11)
                                (begin
                                  ;; kill the tree, figuratively
                                  (remove-component! tree growth-rate)
                                  (system-execute growing-things)
                                  #t)
                                (= (entity-component pillar height) 10)
                                ;; the tree should not have grown.
                                (= (entity-component tree height) 11)
                                (begin
                                  ;; revive the tree, figuratively
                                  (add-component! tree growth-rate 5)
                                  (system-execute growing-things)
                                  #t)
                                (= (entity-component pillar height) 10)
                                ;; the tree should not have grown.
                                (= (entity-component tree height) 16))))
               (!reset-scran!)
               result)))
