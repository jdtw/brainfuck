;;;; package.lisp

(defpackage #:brainfuck
  (:use #:cl)
  (:export #:bf-state
           #:bf-eval
           #:bf-repl
           #:bf-body
           #:bf-compile
           #:bf-compile-file))

