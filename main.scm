(import
  scheme
  (chicken base)
  (chicken file)
  (chicken file posix)
  (chicken flonum)
  (chicken io)
  (chicken irregex)
  (chicken memory)
  (chicken pathname)
  (chicken process-context)
  (chicken random)
  (chicken sort)
  (chicken file posix)
  (chicken condition)
  (prefix inotify #:ino)
  (prefix opengl-glew #:gl)
  (prefix glfw3 #:glfw)
  (prefix gl-math #:glm)
  (prefix gl-utils-core #:glu)
  (prefix gl-utils-mesh #:glu)
  tween
  srfi-1
  srfi-4
  srfi-18
  srfi-71)

(cond-expand (csi (import live-define)) (else))

#;(include-relative "live.scm")

(define *dt* 0)
(define *t* 0)
(define *window*)

(define (init)
  (ino:init!)
  (on-exit ino:clean-up!)
  (glfw:init)
  (glfw:make-window 500 500 "Autumn 2018 Lisp Jam"
                    client-api: glfw:+opengl-es-api+
                    context-version-major: 2
                    context-version-minor: 0
                    swap-interval: 0)
  (set! *window* (glfw:window))
  (print "GLFW: " (glfw:get-version-string))
  (print "OpenGL: " (gl:get-string gl:+version+))
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (main))

(define (main)
  (let* ((now (glfw:get-time))
         (dt (- now *t*)))
    (thread-sleep! 0.016)
    (set! *t* now)
    (set! *dt* dt)
    (update)
    (gl:clear-color 0.5 0.5 0.5 1.0)
    (gl:clear gl:+color-buffer-bit+)
    (render)
    (glfw:swap-buffers *window*)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (main))))


(define (update) (void))
(define (render) (void))
(define (resize . _) (void))

(glfw:framebuffer-size-callback (lambda args (apply resize args)))

(define game-thread (thread-start! init))
(thread-yield!)



(define (resize-current . _)
  (let ((w h (glfw:get-framebuffer-size *window*)))
    (resize *window* w h)))


(define simple-vshader-src #<<EOF
precision highp float;

attribute vec4 position;
attribute vec2 texcoord;

varying vec4 theCoord;
varying vec2 theTexcoord;

uniform vec2 resolution;
uniform vec3 translation;

void main() {
    float scale = 50.0;
    mat3 mat;
    mat[0] = vec3(scale/resolution.x, 0.0, 0.0);
    mat[1] = vec3(0.0, scale/resolution.y, 0.0);
    mat[2] = vec3(0.0);

    gl_Position = vec4((mat * position.xyz) + translation, position.w);
    theCoord = position;
    theTexcoord = texcoord;
}
EOF
)

(define simple-fshader-src #<<EOF
precision highp float;

uniform float time;
uniform vec2 resolution;
uniform vec2 joystick;

varying vec4 theCoord;

mat2 rotate2d(float _angle){
    return mat2(cos(_angle),-sin(_angle),
                sin(_angle),cos(_angle));
}

float circle(vec2 st) {
    return smoothstep(0.0, 0.1, 1.0-distance(st, vec2(0.0)));
}

void main() {
    vec2 pt = theCoord.xy;
    float big = circle(pt);
    float small = circle((pt - joystick) * 5.0);

    vec3 color = (big - small) * vec3(0.7, 0.8, 0.9) + small * vec3(.5, .2, .2);
    gl_FragColor = vec4(color, big);
}
EOF
)

(define square-fshader-src #<<EOF
precision highp float;

uniform float time;
uniform vec2 resolution;
uniform vec2 joystick;

varying vec4 theCoord;

void main() {
  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
EOF
)

(define simple-vshader (glu:make-shader gl:+vertex-shader+ simple-vshader-src))
(define simple-fshader (glu:make-shader gl:+fragment-shader+ simple-fshader-src))
(define square-fshader (glu:make-shader gl:+fragment-shader+ square-fshader-src))
(define simple-program (glu:make-program (list simple-vshader simple-fshader)))
(define square-program (glu:make-program (list simple-vshader square-fshader)))
(resize-current)


(when (eqv? 'terminated (thread-state game-thread))
  (set! game-thread
    (thread-start! (lambda () (glfw:window *window*) (main)))))

;; TODO integrate that to the glfw3 egg
(define (joy-ref joy axis)
  (let* ((base-ptr max (glfw:get-joystick-axes joy))
         (ptr (pointer+ base-ptr (* 4 axis))))
    (assert (< axis max))
    (pointer-f32-ref ptr)))

(define +attributes+ '((position . 0)
                       (texcoord . 1)))
(define +mesh+
  (glu:make-mesh vertices:
             '(attributes: ((position #:float 4)
                            (texcoord #:float 2))
               initial-elements: ((position . (1 -1 0 1
                                               1 1 0 1
                                               -1 1 0 1
                                               -1 -1 0 1))
                                  (texcoord . (1 0
                                               1 1
                                               0 1
                                               0 0))))
             indices: '(type: #:ushort
                        initial-elements: (0 1 2
                                           0 2 3))))

(glu:mesh-make-vao! +mesh+ +attributes+)

(define (resize window width height)
  (gl:viewport 0 0 width height)
  (gl:use-program simple-program)
  (gl:uniform2f (gl:get-uniform-location simple-program
                                         "resolution")
                width height)
  (gl:use-program square-program)
  (gl:uniform2f (gl:get-uniform-location square-program
                                         "resolution")
                width height))


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

(define (get-joystick-buttons joy)
  (let ((ptr len (glfw:get-joystick-buttons joy)))
    (pointer->vector ptr len 1 pointer-u8-ref)))

(define (get-joystick-axes joy)
  (let ((ptr len (glfw:get-joystick-axes joy)))
    (pointer->vector ptr len 4 pointer-f32-ref)))

(define *translation* (glm:make-point 0 0 0))
(define *target* (glm:make-point 0 0 0))
(define check? #t)


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

    (gl:use-program square-program)
    (gl:uniform3fv (gl:get-uniform-location square-program "translation")
                   1 *translation*)
    (gl:use-program simple-program)
    (gl:uniform2fv (gl:get-uniform-location simple-program "joystick")
                   1 joy-v)
    (gl:uniform3fv (gl:get-uniform-location simple-program "translation")
                   1 *target*)))

(define tween-duration 1.0)

(define (ease . args)
  (apply quadratic-ease args))

(define (make-update-motion new-translation new-target)
  (let ((t0 *t*)
        (prev-translation *translation*)
        (prev-target *target*))
    (lambda ()
      (let ((lerp-factor (/ (- *t* t0) tween-duration)))
        (cond ((<= lerp-factor 1.0)
               (set! *translation* (glm:lerp prev-translation new-translation
                                             (tween ease 'out 0. 1. lerp-factor))))
              (else
                (set! *translation* new-translation)
                (set! *target* new-target)
                #;(set! update (make-update-motion prev-translation prev-target))
                (set! update update-idle))))
      (gl:use-program square-program)
      (gl:uniform3fv (gl:get-uniform-location simple-program "translation")
                     1 *translation*))))

#;(make-update-motion (glm:make-point 0.5 0.5 0) (glm:make-point 0 0 0))
#;(define update update-idle)


(define (render)
  (gl:bind-buffer gl:+array-buffer+ (glu:mesh-vertex-buffer +mesh+))
  (let ((locations +attributes+))
    (for-each
      (lambda (attrib)
        (let ((loc (alist-ref (glu:vertex-attribute-name attrib)
                              locations))
              (stride (glu:mesh-stride +mesh+)))
          (gl:vertex-attrib-pointer loc
                                    (glu:vertex-attribute-number attrib)
                                    (glu:type->gl (glu:vertex-attribute-type attrib))
                                    (glu:vertex-attribute-normalized attrib)
                                    stride
                                    (address->pointer (glu:vertex-attribute-offset attrib)))
          (gl:enable-vertex-attrib-array loc)))
      (glu:mesh-vertex-attributes +mesh+)))
  
  (gl:bind-buffer gl:+element-array-buffer+ (glu:mesh-index-buffer +mesh+))

  (gl:use-program square-program)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f)
  (gl:use-program simple-program)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f)
  )

(set! update update-idle)
(resize-current)

(cond-expand ((or chicken-script compiling) (thread-join! game-thread))
             (else))
