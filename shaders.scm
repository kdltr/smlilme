;; Vertex shaders
;; ==============

(define simple-vshader-src #<<EOF
#version 100
precision highp float;

attribute vec4 position;
attribute vec2 texcoord;

varying vec4 theCoord;
varying vec2 theTexcoord;

uniform vec2 resolution;
uniform float scale;
uniform vec3 translation;

void main() {
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


;; Fragment shaders
;; ================

(define target-fshader-src #<<EOF
#version 100
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
    gl_FragColor = vec4(color, big * 0.5);
}
EOF
)

(define quad-fshader-src #<<EOF
#version 100
precision highp float;
  
uniform sampler2D theTexture;
uniform float flip;

varying vec2 theTexcoord;

void main() {
  vec2 coord = vec2(theTexcoord.x, flip * theTexcoord.y);
  gl_FragColor = texture2D(theTexture, coord);
}
EOF
)


;; Programs
;; ========

(define simple-vshader (glu:make-shader gl:+vertex-shader+ simple-vshader-src))

(define target-fshader (glu:make-shader gl:+fragment-shader+ target-fshader-src))
(define quad-fshader (glu:make-shader gl:+fragment-shader+ quad-fshader-src))

(define target-program (glu:make-program (list simple-vshader target-fshader)))
(define quad-program (glu:make-program (list simple-vshader quad-fshader)))
