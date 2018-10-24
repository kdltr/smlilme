;; Constants
;; =========

(define starting-position-pixel #u8(255 0 0))
(define object-position-pixel #u8(0 255 0))
(define npc-position-pixel #u8(0 0 255))

(define wall-zone-pixel #u8(0 0 0))
(define grass-zone-pixel #u8(255 0 255))
(define water-zone-pixel #u8(255 128 0))
(define object-trigger-pixel #u8(0 255 255))
(define npc-trigger-pixel #u8(255 255 0))

(define tween-duration 1.0)


;; State Variables
;; ===============

(define *translation* (glm:make-point 0 0 0))
(define *target* (glm:make-point 0 0 0))


;; State Machine
;; =============

(define (update-idle)
  (let* ((axes (get-joystick-axes 0))
         (buts (get-joystick-buttons 0))
         (joy-v (joy-improve (vector-ref axes 0)
                             (- (vector-ref axes 1)))))
    (when (= 1 (vector-ref buts 0))
      (let* ((new-translation (glm:v+ *translation*
                                      (glm:v+ (glm:v- *target* *translation*)
                                              (glm:v* joy-v (fp/ 1. 25.))))) ;; TODO target radius
             (new-target (glm:v+ new-translation
                                 (glm:v- new-translation *translation*))))
        (set! update (make-update-motion new-translation new-target))))

    (gl:use-program quad-program)
    (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                   1 *translation*)
    (gl:use-program target-program)
    (gl:uniform2fv (gl:get-uniform-location target-program "joystick")
                   1 joy-v)
    (gl:uniform3fv (gl:get-uniform-location target-program "translation")
                   1 *target*)))


(define (make-update-motion new-translation new-target)
  (let ((t0 *t*)
        (prev-translation *translation*)
        (prev-target *target*))
    (lambda ()
      (let ((lerp-factor (/ (- *t* t0) tween-duration)))
        (cond ((<= lerp-factor 1.0)
               (set! *translation* (glm:lerp prev-translation new-translation
                                             (tween quadratic-ease 'out 0. 1. lerp-factor))))
              (else
                (set! *translation* new-translation)
                (set! *target* new-target)
                (set! update update-idle))))
      (gl:use-program quad-program)
      (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                     1 *translation*))))


;; initial state
(set! update update-idle)
