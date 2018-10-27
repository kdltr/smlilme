;; Offscreen rendering

(define *clear-color* '(0.914 0.749 0.757 1.0))
(define *framebuffer* (glu:gen-framebuffer))
(define *rendering-texture* (glu:gen-texture))

(glu:with-framebuffer *framebuffer*
  (gl:bind-texture gl:+texture-2d+ *rendering-texture*)
  (gl:tex-image-2d gl:+texture-2d+ 0 gl:+rgb+ 1920 1080 0 gl:+rgb+ gl:+unsigned-byte+ #f)
  (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+linear+)
  (gl:tex-parameteri gl:+texture-2d+ gl:+texture-mag-filter+ gl:+linear+)
  (gl:framebuffer-texture-2d gl:+framebuffer+
                             gl:+color-attachment0+
                             gl:+texture-2d+
                             *rendering-texture*
                             0)
  (unless (eqv? (gl:check-framebuffer-status gl:+framebuffer+)
                gl:+framebuffer-complete+)
    (error "Could not create offscreen framebuffer")))
  

;; Textures
;; ========

(define *player-texture* (load-texture (make-pathname '("resources" "gras_fik") "player.png")))

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
  
  (glu:with-framebuffer *framebuffer*
    (gl:viewport 0 0 1920 1080)
    (draw-game))
  
  (let ((width height (if (< (/ *window-width* *window-height*) 16/9)
                          (values *window-width* (round (/ *window-width* 16/9)))
                          (values (round (* *window-height* 16/9)) *window-height*))))
    (gl:viewport (round (/ (- *window-width* width) 2))
                 (round (/ (- *window-height* height) 2))
                 width height))
  
  (apply gl:clear-color *clear-color*)
  (gl:clear gl:+color-buffer-bit+)
  (glu:with-texture gl:+texture-2d+ *rendering-texture*
    (gl:use-program quad-program)
    (gl:uniform2f (gl:get-uniform-location quad-program "resolution")
                  1.0 1.0)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  1.0)
    (gl:uniform3f (gl:get-uniform-location quad-program "translation")
                  0. 0. 0.)
    (draw!))
  )

(define (draw-game)
  ;; Background
  (glu:with-texture gl:+texture-2d+ *background-texture*
    (gl:use-program quad-program)
    (gl:uniform2f (gl:get-uniform-location quad-program "resolution")
                  1.0 1.0)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  1.0)
    (gl:uniform3f (gl:get-uniform-location quad-program "translation")
                  0. 0. 0.)
    (draw!))
  
  ;; Character
  (glu:with-texture gl:+texture-2d+ *player-texture*
    (gl:use-program quad-program)
    (gl:uniform2f (gl:get-uniform-location quad-program "resolution")
                  1920. 1080.)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  113.)
    (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                   1 *translation*)
    (draw!))
  
  ;; Item
  (glu:with-texture gl:+texture-2d+ *item-texture*
    (gl:use-program quad-program)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  113.)
    (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                   1 *item-position*)
    (draw!))
  
  ;; NPC
  (glu:with-texture gl:+texture-2d+ *npc-texture*
    (gl:use-program quad-program)
    (gl:uniform1f (gl:get-uniform-location quad-program "scale")
                  113.)
    (gl:uniform3fv (gl:get-uniform-location quad-program "translation")
                   1 *npc-position*)
    (draw!))
  
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
  (gl:use-program 0))


(define (draw!)
  (gl:draw-elements (glu:mode->gl (glu:mesh-mode +mesh+))
                    (glu:mesh-n-indices +mesh+)
                    (glu:type->gl (glu:mesh-index-type +mesh+))
                    #f))
