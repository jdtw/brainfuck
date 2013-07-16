This is a [brainfuck][wiki]
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

    TODO

### bf-repl

    TODO

### bf-body

    TODO

### bf-compile

    TODO

[wiki]: http://en.wikipedia.org/wiki/Brainfuck
[nice]: http://www.muppetlabs.com/~breadbox/bf/standards.html

