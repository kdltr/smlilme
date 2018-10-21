(module sound

 (startup
  shutdown
  op_open_memory
  set-channel-stream!
  set-channel-volume!
  set-channel-state!
  +paused+
  +stopping+
  +playing+)

(import scheme (chicken foreign) (chicken blob))

(foreign-declare "#include \"sound-impl.c\"")

(define startup
  (foreign-lambda void "startup" ))

(define shutdown
  (foreign-lambda void "shutdown"))

(define (op_open_memory blob)
  ((foreign-lambda* c-pointer ((nonnull-blob data) (size_t size))
                   "C_return(op_open_memory(data, size, NULL));")
   blob (blob-size blob)))

(define set-channel-stream!
  (foreign-lambda* void ((int i) (c-pointer s))
                   "channels[i].stream = (OggOpusFile*) s;"))

(define set-channel-volume!
  (foreign-lambda* void ((int i) (float v))
                   "channels[i].volume = v;"))

(define set-channel-state!
  (foreign-lambda* void ((int i) (int s))
                   "channels[i].state = s;"))

(define +paused+ (foreign-value "PAUSED" int))
(define +stopping+ (foreign-value "STOPPING" int))
(define +playing+ (foreign-value "PLAYING" int))

)
