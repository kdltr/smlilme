(import
  scheme
  (chicken base)
  (chicken file)
  (chicken file posix)
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
  gl-utils-core
  srfi-1
  srfi-4
  srfi-18
  srfi-71
  (prefix pseudofs #:fs))

(include-relative "debug.scm")
(include-relative "live.scm")
(include-relative "subscriptions.scm")

(define *dt* 0)
(define *t* 0)
(define *window*)

(glfw:framebuffer-size-callback
  (lambda (window width height)
    (gl:viewport 0 0 width height)))

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
  (mount-pseudofs "." "resources" '())
  (init-fs-subscriptions)
  (main))

(define (main)
  (let* ((now (glfw:get-time))
         (dt (- now *t*)))
    (set! *t* now)
    (set! *dt* dt)
    (gl:clear-color 0.5 0.5 0.5 1.0)
    (gl:clear gl:+color-buffer-bit+)
    (glfw:swap-buffers *window*)
    (thread-sleep! 0.016)
    (watch-for-fs-changes)
    (glfw:poll-events)
    (unless (glfw:window-should-close (glfw:window))
      (main))))


(define game-thread (thread-start! init))

(cond-expand ((or chicken-script compiling) (thread-join! game-thread))
             (else))
