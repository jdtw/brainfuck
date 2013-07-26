This is a (naive) [brainfuck][wiki]
implementation written in Common Lisp. 

## Implementation details

This implementation is a [nice][nice] implementation:

* It uses 30,000 cells.
* If a program attempts to move beyond the bounds of those 30,000
  cells, it will raise an error.
* The cell values are of type `'(unsigned-byte 8)`.
* If a program attempts to increment the value of a cell above the
  maximum, or decrement a value below the minimum, it will wrap
  around.
* If a program contains unbalanced brackets, an error will be raised. 

## Quirks

When EOF is read, the cell at the pointer is set to 0 (as opposed to
the other popular options of -1 or "no change".)

## API

### bf-eval

    (bf-eval state)
    
For example, at the REPL,

    BRAINFUCK> (defparameter *hello-world* "++++++++++[>+++++++>++++++++++>
    +++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++
    ++++++++++++.>.+++.------.--------.>+.>.")
    *HELLO-WORLD*
    BRAINFUCK> (bf-eval (make-bf-state :program *hello-world*))
    Hello World!
    NIL

### bf-repl

    (bf-repl) 

`(bf-repl)` will start a REPL where you can enter brainfuck programs.
Entered programs must be on a single line.

### bf-body

    (bf-body program in out)
    
`bf-body` is really a helper function for `bf-compile` below, but it
us useful for testing. It returns the s-expression that is created
from the brainfuck program. 

For example:

    (bf-body *hello-world* *standard-input* *standard-output*)

### bf-compile

    (bf-compile program &key (in *standard-input*) (out *standard-output*))
    
Returns a compiled function. For example:

    * (funcall (bf-compile *hello-world*))
    Hello, World!
    
### bf-compile-file

    (bf-compile-file path &key (in *standard-input*) (out *standard-output*))

Returns a compiled function. For example:

    * (bf-compile-file #p"bf/hello-world.lisp")
    Hello, world!
    
## sbcl vs. ccl

On Win8 x64, the windows fork of sbcl requires a **lot** of time and
memory to compile 99-bottles.bf:

    $ sbcl --dynamic-space-size 2048
    This is SBCL 1.1.4.0.mswin.1288-90ab477, an implementation of ANSI Common Lisp.

    * (bf-compile-file #p"bf/99-bottles.bf") ;; bf-compile-file calls
                                             ;; (time) internally.

    Evaluation took:
      16.207 seconds of real time
      16.171875 seconds of total run time (13.875000 user, 2.296875 system)
      [ Run times consist of 6.179 seconds GC time, and 9.993 seconds non-GC time. ]
    
      99.78% CPU
      23,617 lambdas converted
      38,808,919,242 processor cycles
      2,837,170,720 bytes consed

Whereas ccl has no problems.

    $ ccl
    Welcome to Clozure Common Lisp Version 1.9-r15765  (WindowsX8664)!
    
    * (bf-compile-file #p"bf/99-bottles.bf") ;; bf-compile-file calls
                                             ;; (time) internally.
                                             
    (COMPILE NIL (BF-BODY PROGRAM IN OUT))
    took 1,814,000 microseconds (1.814000 seconds) to run.
           143,096 microseconds (0.143096 seconds, 7.89%) of which was spent in GC.
    During that period, and with 8 available CPU cores,
         1,796,875 microseconds (1.796875 seconds) were spent in user mode
            15,625 microseconds (0.015625 seconds) were spent in system mode
     48,043,200 bytes of memory allocated.
     
## License

MIT

[wiki]: http://en.wikipedia.org/wiki/Brainfuck
[nice]: http://www.muppetlabs.com/~breadbox/bf/standards.html

