;; Textures
;; ========

(define background-texture
  (load-texture "resources/gras_fik/lvl1.png"))


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
  
  (gl:viewport 0 0 1920 1080)
  
  ;; Background
  (glu:with-texture gl:+texture-2d+ background-texture
    (gl:use-program quad-program)
    (gl:uniform2f (gl:get-uniform-location quad-program "resolution")
                  1.0 1.0)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  1.0)
    (gl:uniform3f (gl:get-uniform-location quad-program "translation")
                  0. 0. 0.)
    (draw!))
  
  ;; Character
  (gl:use-program quad-program)
  (gl:uniform2f (gl:get-uniform-location quad-program "resolution")
                1920. 1080.)
  (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                50.)
  (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                 1 *translation*)
  (draw!)
  
  ;; Target
  (gl:use-program target-program)
  (gl:uniform2f (gl:get-uniform-location target-program "resolution")
                1920. 1080.)
  (gl:uniform1f (gl:get-uniform-location target-program "scale")
                50.)
  (gl:uniform3fv (gl:get-uniform-location target-program "translation")
                 1 *target*)
  (gl:uniform2fv (gl:get-uniform-location target-program "joystick")
                 1 *joystick*)
  (draw!)
  (gl:use-program 0)
  )

(define (draw!)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f))
