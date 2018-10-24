
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

  (gl:use-program quad-program)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f)
  (gl:use-program target-program)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f)
  )
