(ql:quickload :jin-scripts)

(sb-ext:save-lisp-and-die
 "/Users/jin/.local/bin/cl"
 :toplevel 'jin-scripts:main
 :executable t)
