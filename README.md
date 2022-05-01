# simplisp
A meta recursive interpreter

Includes trace options, but with Racket CS the options\
trace-value trace-assgn and trace-varef\
cannot be used when simplisp traces its own source code.

Works well with Racket BC and Racket versions up to and including 7.9.\
Problem with prop:object-name in Racket CS 8.0 and up.\
Matthew Flatt has found the cause of the problem and has promised to fix it.
