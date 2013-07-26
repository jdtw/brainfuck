;;;; brainfuck.asd

(asdf:defsystem #:brainfuck
  :serial t
  :description "brainfuck"
  :author "John Wood <j@jdtw.us>"
  :license "MIT"
  :components ((:file "package")
               (:file "brainfuck")))

