
(define *center-dead-zone* 0.25)
(define *border-dead-zone* 0.9)

(define deadzone-func
  (let* ((x1 *center-dead-zone*)
         (y1 0.0)
         (x2 *border-dead-zone*)
         (y2 1.0)
         (a (fp/ (fp- y2 y1) (fp- x2 x1)))
         (b (fp/ (fp- (fp* x2 y1) (fp* x1 y2))
                 (fp- x2 x1))))
    (lambda (x)
      (fp+ (fp* a x) b))))

(define (joy-improve x y)
  (let* ((v (glm:make-point x y 0))
         (mag (glm:vector-magnitude v))
         (_ (glm:normalize! v))
         (better-mag (if (<= mag *center-dead-zone*)
                         0.0
                         (if (>= mag *border-dead-zone*)
                             1.0
                             (deadzone-func mag))))
         (v (glm:v* v better-mag)))
    v))

(define (pointer->vector ptr len datum-size ref)
  (let* ((v (make-vector len)))
    (do ((i 0 (add1 i))
         (p ptr (pointer+ p datum-size)))
        ((= i len) v)
        (vector-set! v i (ref p)))))

;; TODO integrate that to the glfw3 egg
(define (get-joystick-buttons joy)
  (let ((ptr len (glfw:get-joystick-buttons joy)))
    (pointer->vector ptr len 1 pointer-u8-ref)))

;; TODO integrate that to the glfw3 egg
(define (get-joystick-axes joy)
  (let ((ptr len (glfw:get-joystick-axes joy)))
    (pointer->vector ptr len 4 pointer-f32-ref)))


(define (load-texture filename)
  (let ((data width height channels
          (with-input-from-file filename img:read-image))
        (texture-id (glu:gen-texture)))
    (assert (= channels 3))
    (glu:with-texture gl:+texture-2d+ texture-id
      (gl:tex-parameteri gl:+texture-2d+
                         gl:+texture-wrap-s+ gl:+clamp-to-edge+)
      (gl:tex-parameteri gl:+texture-2d+
                         gl:+texture-wrap-t+ gl:+clamp-to-edge+)
      (gl:tex-parameteri gl:+texture-2d+
                         gl:+texture-min-filter+ gl:+linear-mipmap-linear+)
      (gl:tex-parameteri gl:+texture-2d+
                         gl:+texture-mag-filter+ gl:+linear+)
                         
      (gl:tex-image-2d gl:+texture-2d+
                       0
                       gl:+rgb+
                       width
                       height
                       0
                       gl:+rgb+
                       gl:+unsigned-byte+
                       (glu:->pointer data))
      (gl:generate-mipmap gl:+texture-2d+))
    texture-id))

(define (find-pixel-index data reference-pixel)
  (let lp ((i 0))
    (let ((pixel (subu8vector data i (+ i 3))))
      (if (equal? pixel reference-pixel)
          i
          (lp (+ i 3))))))

(define (index->pos i width channels)
  (let ((y x (quotient&remainder (/ i channels) width)))
    (values x y)))

(define (image-ref data pt)
  (let* ((x (inexact->exact (round (glm:point-x pt))))
         (y (inexact->exact (round (glm:point-y pt))))
         (i (* (+ (* y 1920) x) 3)))
    (subu8vector data i (+ i 3))))

(define (world->view pt)
  (let ((x (glm:point-x pt))
        (y (glm:point-y pt)))
    (glm:make-point (- (/ x 960.) 1) (- (- (/ y 540.) 1)) 0)))

(define (view->world pt)
  (let ((x (glm:point-x pt))
        (y (glm:point-y pt)))
    (glm:make-point (round (* (+ x 1) 960.))
                    (round (* (+ (- y) 1) 540.))
                    0)))

(define (ray-cast origin increment max-magnitude pixel-searched)
  (let lp ((pt origin))
    (cond ((or (zero? max-magnitude)
               (> (glm:vector-magnitude (glm:v- pt origin)) max-magnitude))
           #f)
          ((equal? (image-ref *collision-map* pt)
                   pixel-searched)
           (glm:v- pt increment))
          (else
            (lp (glm:v+ pt increment))))))
