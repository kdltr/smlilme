;; Constants
;; =========

(define starting-position-pixel #u8(255 0 0))
(define item-position-pixel #u8(0 255 0))
(define npc-position-pixel #u8(0 0 255))

(define wall-zone-pixel #u8(0 0 0))
(define grass-zone-pixel #u8(255 0 255))
(define water-zone-pixel #u8(255 128 0))
(define item-trigger-pixel #u8(0 255 255))
(define npc-trigger-pixel #u8(255 255 0))

(define tween-duration 1.0)


;; State Variables
;; ===============

(define *level*
  (cond-expand ((or script compiling)
                (if (pair? (command-line-arguments))
                    (string->number (car (command-line-arguments)))
                    1))
                (else 1)))

(define *background-texture* 0)
(define *item-texture* 0)
(define *npc-texture* 0)

(define *item-following* #f)

(define *item-initial-position* (glm:make-point 0 0 0))
(define *item-position* (glm:make-point 0 0 0))
(define *npc-position* (glm:make-point 0 0 0))

(define *translation* (glm:make-point 0 0 0))
(define *target* (glm:make-point 0 0 0))
(define *joystick* (glm:make-point 0 0 0))

(define *collision-map*)

;; State Machine
;; =============

(define (update-startup)
  (let* ((data width height channels
           (with-input-from-file (resource-path *level* "collision.png")
             img:read-image)))
    (assert (= width 1920))
    (assert (= height 1080))
    (assert (= channels 3))
    (set! *collision-map* data)
    (let* ((i (find-pixel-index data starting-position-pixel))
           (x y (index->pos i width channels))
           (pt (world->view (glm:make-point x y 0))))
      (set! *translation* pt)
      (set! *target* pt))
    (let* ((i (find-pixel-index data item-position-pixel))
           (x y (index->pos i width channels))
           (pt (world->view (glm:make-point x y 0))))
      (set! *item-initial-position* pt)
      (set! *item-position* pt))
    (let* ((i (find-pixel-index data npc-position-pixel))
           (x y (index->pos i width channels))
           (pt (world->view (glm:make-point x y 0))))
      (set! *npc-position* pt))
    (set! *background-texture* (load-texture (resource-path *level* "background.png")))
    (set! *item-texture* (load-texture (resource-path *level* "item.png")))
    (set! *npc-texture* (load-texture (resource-path *level* "npc.png")))
    (snd:set-channel-volume! (sub1 *level*) 1.0)
    (set! update update-idle)))

(define (update-idle)
  (let* ((axes (get-joystick-axes 0))
         (buts (get-joystick-buttons 0))
         (joy-v (joy-improve (vector-ref axes 0)
                             (- (vector-ref axes 1))))
         (current-pixel (image-ref *collision-map* (view->world *translation*))))

    (set! *joystick* joy-v)

    (when (= 1 (vector-ref buts 0))
      (let* ((new-translation (glm:v+ *translation*
                                      (glm:v+ (glm:v- *target* *translation*)
                                              (glm:v* joy-v
                                                      (if (equal? current-pixel water-zone-pixel)
                                                          0.0
                                                          (fp/ 1. 50.)))))) ;; TODO target radius

             (new-pixel (image-ref *collision-map* (view->world new-translation)))
             (new-target (glm:v+ new-translation
                                 (glm:v* (glm:v- new-translation *translation*)
                                         (if (equal? new-pixel grass-zone-pixel)
                                             0.5
                                             1)))))
        (set! update (make-update-motion new-translation new-target))))))


(define (make-update-motion new-translation new-target)
  (let* ((t0 *t*)
         (orig (view->world *translation*))
         (dest (view->world new-translation))
         (increment (glm:v- dest orig))
         (mag (glm:vector-magnitude increment))
         (_ (glm:normalize! increment))
         (cast (ray-cast orig increment mag wall-zone-pixel))
         (new-translation (if cast (world->view cast) new-translation))
         (new-target (if cast (world->view cast) new-target))
         (easing (if cast bounce-ease quadratic-ease))
         (prev-translation *translation*)
         (prev-target *target*)
         (prev-item *item-position*))
    (lambda ()
      (let ((lerp-factor (/ (- *t* t0) tween-duration)))
        (cond ((<= lerp-factor 1.0)
               (set! *translation* (glm:lerp prev-translation new-translation
                                             (tween easing 'out 0. 1. lerp-factor)))
               (when *item-following*
                 (set! *item-position* (glm:lerp prev-item prev-translation
                                                 (tween quadratic-ease 'out 0. 1. lerp-factor)))))
              (else
                (set! *translation* new-translation)
                (set! *target* new-target)
                (when *item-following* (set! *item-position* prev-translation))
                (when cast
                  (set! *item-position* *item-initial-position*)
                  (set! *item-following* #f))
                (set! update update-idle)))

        (let ((current-pixel (image-ref *collision-map* (view->world *translation*))))
          (when (and *item-following* (equal? current-pixel npc-trigger-pixel))
            (set! update update-level-end)) ;; level end
          (when (equal? current-pixel item-trigger-pixel)
            (set! *item-following* #t)))
        ))))

(define (update-level-end)
  ;; animation + music trigger
  (set! *level* (add1 *level*))
  (set! *item-following* #f)
  (set! update update-startup))


;; initial state
(set! update update-startup)
