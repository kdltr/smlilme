(module glfw3-bindings *

(import (chicken base) scheme (chicken foreign) bind)

(bind-rename/pattern "^GLFW_([A-Z].+)$" "+\\1+")
(bind-rename/pattern "(.*)GLFW(.+)$" "\\1\\2")
(bind-rename/pattern "glfw(.+)$" "\\1")
(bind-options default-renaming: ""
              export-constants: #t)

(bind-file* "glfw3.h")

) ; end glfw3-bindings
