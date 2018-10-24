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
  (prefix stb-image #:img)
  tween
  srfi-1
  srfi-4
  srfi-18
  srfi-71)

(cond-expand (csi (import live-define)) (else))

(define *dt* 0)
(define *t* 0)
(define *window*)

(define (init)
  (ino:init!)
  (on-exit ino:clean-up!)
  (glfw:init)
  (glfw:make-window 640 480 "Autumn 2018 Lisp Jam"
                    client-api: glfw:+opengl-es-api+
                    context-version-major: 2
                    context-version-minor: 0
                    swap-interval: 0)
  (set! *window* (glfw:window))
  (print "GLFW: " (glfw:get-version-string))
  (print "OpenGL: " (gl:get-string gl:+version+))
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+))

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
(define (prepare) (void))
(glfw:framebuffer-size-callback (lambda (win w h) (resize win w h)))


(init)

(include-relative "utilities.scm")
(include-relative "shaders.scm")
(include-relative "mesh.scm")
(include-relative "game.scm")
(include-relative "rendering.scm")

(prepare)

(define game-thread (thread-start! main))

(cond-expand ((or chicken-script compiling) (thread-join! game-thread))
             (else))

#;
(when (eqv? 'terminated (thread-state game-thread))
  (set! game-thread (thread-start! main)))

(define (resize window width height)
  (let ((width 1920)
        (height 1080))
  (gl:viewport 0 0 width height)
  (gl:use-program target-program)
  (gl:uniform2f (gl:get-uniform-location target-program
                                         "resolution")
                width height)
  (gl:use-program quad-program)
  (gl:uniform2f (gl:get-uniform-location quad-program
                                         "resolution")
                width height)))

