(module pseudofs (read write mkdir ls type subscribe reset!)

(import (except scheme read write) (chicken base) srfi-1)

(include-relative "../../debug.scm")

(define-record node directory? subscribers content)

(define (make-directory-node)
  (make-node #t '() '()))

(define (make-file-node content)
  (make-node #f '() content))

(define *root* (make-directory-node))


;; Internal definitions

(define (search node path)
  (if (null? path)
      node
      (begin
        (assert (node-directory? node))
        (and-let* ((next-node (alist-ref (car path) (node-content node))))
             (search next-node (cdr path))))))

(define (create path new-node)
  (debug "create: ~s ~s" path new-node)
  (let* ((parent (butlast path))
         (target (last path))
         (node (search *root* (butlast path))))
    (unless node
      (error "Parents do not exist" parent))
    (unless (node-directory? node)
      (error "Parent node is not a directory" parent))
    (let* ((content (node-content node))
           (maybe-file (alist-ref target content)))
      (when maybe-file
        (error "Node already exists" path))
      (node-content-set!
        node
        (cons (cons target new-node)
              content))
      (notify-subscribers node parent target))))

(define (notify-subscribers node path message)
  (debug "notify-subscribers: ~s ~s" path message)
  (for-each (lambda (proc) (proc path message))
    (node-subscribers node)))


;; User API

(define (mkdir path)
  (create path (make-directory-node)))

(define (read path)
  (let ((maybe-node (search *root* path)))
    (unless maybe-node
      (error "No such file or directory" path))
    (when (node-directory? maybe-node)
      (error "Cannot read a directory node" path))
    (node-content maybe-node)))

(define (write path content)
  (let ((file (search *root* path)))
    (if file
        (begin
          (node-content-set! file content)
          (notify-subscribers file path content))
        (create path (make-file-node content)))))

(define (ls path)
  (let ((node (search *root* path)))
    (unless node
      (error "No such file or directory" path))
    (unless (node-directory? node)
      (error "Node is not a directory" path))
    (map car (node-content node))))

(define (type path)
  (let ((node (search *root* path)))
    (unless node
      (error "No such file or directory" path))
    (if (node-directory? node)
        'directory
        'file)))

(define (subscribe path proc)
  (let ((node (search *root* path)))
    (unless node
      (error "No such file or directory" path))
    (node-subscribers-set! node
      (cons proc (node-subscribers node)))))

(define (reset!)
  (set! *root* (make-directory-node)))

) ; module pseudofs
