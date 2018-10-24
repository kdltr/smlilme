
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
