(define (init-fs-subscriptions)
  (fs:mkdir '(shaders))
  (fs:mkdir '(shaders objects))
  (fs:mkdir '(shaders objects vertex))
  (fs:mkdir '(shaders objects fragment))
  (fs:mkdir '(shaders programs))
  
  (init-subdir-subscriptions '(resources shaders vertex) (reload-shader 'vertex))
  (init-subdir-subscriptions '(resources shaders fragment) (reload-shader 'fragment))
  (init-subdir-subscriptions '(resources shaders programs) reload-program))

(define (init-subdir-subscriptions subdir-path proc)
  (fs:subscribe subdir-path
                (lambda (path filename)
                  (load-new-file subdir-path filename proc)))
  (for-each
    (lambda (filename)
      (load-new-file subdir-path filename proc))
    (fs:ls subdir-path)))

(define (load-new-file subdir-path filename loader)
  (debug "load-new-file: ~s ~s ~s" subdir-path filename loader)
  (let ((file-path `(,@subdir-path ,filename)))
    (fs:subscribe file-path loader)
    (loader file-path (fs:read file-path))))

;; Shader stuff

(define (shader-type->gl type)
  (case type
    ((vertex) gl:+vertex-shader+)
    ((fragment) gl:+fragment-shader+)))

(define ((reload-shader type) path code)
  (debug "reload-shader: ~s ~s ~s" type path code)
  (let* ((name (last path))
         (shader-path `(shaders objects ,type ,name))
         (old-shader (handle-exceptions exn #f (fs:read shader-path))))
    (when old-shader
      (gl:delete-shader old-shader))
    (fs:write shader-path (make-shader (shader-type->gl type) code))))

(define (reload-program path shaders-list)
  (let* ((name (last path))
         (program-path `(shaders programs ,name))
         (old-program (handle-exceptions exn #f (fs:read program-path)))
         (shaders-id (list (fs:read `(shaders objects vertex ,(car shaders-list)))
                           (fs:read `(shaders objects fragment ,(cadr shaders-list))))))
    (when old-program
      (gl:delete-program old-program))
    (fs:write program-path (make-program shaders-id))))
