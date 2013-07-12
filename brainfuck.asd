;;;; brainfuck.asd

(asdf:defsystem #:brainfuck
  :serial t
  :description "brainfuck"
  :author "John Wood <j@jdtw.us>"
  :license "Simplified BSD"
  :components ((:file "package")
               (:file "brainfuck")))

