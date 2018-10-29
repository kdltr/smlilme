(import
  scheme
  (chicken base)
  (chicken file)
  (chicken file posix)
  (chicken flonum)
  (chicken format)
  (chicken io)
  (chicken irregex)
  (chicken memory)
  (chicken pathname)
  (chicken process-context)
  (chicken random)
  (chicken sort)
  (chicken file posix)
  (chicken condition)
  (prefix opengl-glew #:gl)
  (prefix glfw3 #:glfw)
  (prefix gl-math #:glm)
  (prefix gl-utils-core #:glu)
  (prefix gl-utils-mesh #:glu)
  (prefix stb-image #:img)
  (prefix sound #:snd)
  tween
  srfi-1
  srfi-4
  srfi-18
  srfi-71)

(include-relative "utilities.scm")

(cond-expand (csi (import live-define)) (else))

(define *dt* 0)
(define *t* 0)
(define *window*)

(define *window-width* 1280)
(define *window-height* 720)

(define (init)
  (snd:startup)
  (on-exit snd:shutdown)
  (glfw:init)
  (glfw:make-window *window-width*
                    *window-height*
                    "Autumn 2018 Lisp Jam"
                    ;client-api: glfw:+opengl-core-profile+
                    ;context-version-major: 3
                    ;context-version-minor: 2
                    swap-interval: 0)
  (set! *window* (glfw:window))
  (gl:init)
  (print "GLFW: " (glfw:get-version-string))
  (print "OpenGL: " (gl:get-string gl:+version+))
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+))

(define (main)
  (let* ((now (glfw:get-time))
         (dt (- now *t*)))
    (set! *t* now)
    (set! *dt* dt)
    (update)
    (render)
    (glfw:swap-buffers *window*)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (main))))


(define (update) (void))
(define (render) (void))

(define (resize window width height)
  (set! *window-width* width)
  (set! *window-height* height))

(glfw:framebuffer-size-callback resize)

(define *audio-1-data* (file->blob (make-pathname '("resources" "music") "track1.opus")))
(define *audio-2-data* (file->blob (make-pathname '("resources" "music") "track2.opus")))
(define *audio-3-data* (file->blob (make-pathname '("resources" "music") "track3.opus")))
(define *audio-4-data* (file->blob (make-pathname '("resources" "music") "track4.opus")))

(define *audio-1-stream* (snd:op_open_memory *audio-1-data*))
(define *audio-2-stream* (snd:op_open_memory *audio-2-data*))
(define *audio-3-stream* (snd:op_open_memory *audio-3-data*))
(define *audio-4-stream* (snd:op_open_memory *audio-4-data*))

(snd:set-channel-stream! 0 *audio-1-stream*)
(snd:set-channel-stream! 1 *audio-2-stream*)
(snd:set-channel-stream! 2 *audio-3-stream*)
(snd:set-channel-stream! 3 *audio-4-stream*)

(snd:set-channel-volume! 0 0.0)
(snd:set-channel-volume! 1 0.0)
(snd:set-channel-volume! 2 0.0)
(snd:set-channel-volume! 3 0.0)

(snd:set-channel-state! 0 snd:+playing+)
(snd:set-channel-state! 1 snd:+playing+)
(snd:set-channel-state! 2 snd:+playing+)
(snd:set-channel-state! 3 snd:+playing+)

(init)

(include-relative "shaders.scm")
(include-relative "mesh.scm")
(include-relative "game.scm")
(include-relative "rendering.scm")

(define game-thread)

(if (equal? #() (get-joystick-axes 0))
    (begin
      (fprintf (current-error-port) "No gamepad found! Aborting.~%")
      (exit 1))
    (set! game-thread (thread-start! main)))

(cond-expand ((or chicken-script compiling) (thread-join! game-thread))
             (else))

#;
(when (eqv? 'terminated (thread-state game-thread))
  (set! game-thread (thread-start! main)))
