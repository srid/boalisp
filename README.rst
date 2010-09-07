Boa Lisp
========

The Boa Lisp compiler, written in MzScheme, compiles s-exps to Python
byte-code, which can be executed using the Python interpreter
itself. This would inevitably provide the developer with Macros and
other intuitive powers of Lisp. Thanks to SYNTAX-CASE macros in
Scheme, I could implement the parser, and perhaps macros too, using
the READ function. The rest is trivial. FFI is used to call AST
functions in `libpython` to generate the final byte-code.

Consider the following script, taken from the examples in Boa Lisp:

    (import web)

    (set! urls (:tuple "/(.*)" "hello"))

    (class hello ()
      (def GET (self name)
        (if (not name)
            (set! name "world"))
        (for c in (xrange (int "10"))
             (print "Hello," (+ name "!")))))

    (if (= __name__ "__main__")
        (web.run urls (globals))
        (print "eek!"))

    (set! h (hello))
    (h.GET "")

This is the Hello World [web.py](http://webpy.org/) example. It can be
compiled and run,

    $ mzscheme -r compile.ss hello.boa hello.pyc
    $ python hello.pyc

I think, this is neat so far. Imagine being able to import Boa modules
from Python and vice versa, which can be performed using import
hooks. Although I am being listless nowadays when it comes to this
project, I definitely want to complete the most wanted features like
mapping AST nodes to corresponding forms in s-exp with line number
information which would help in debugging the script. Furthermore,
having able to write Boa apps using the popular frameworks and
libraries like Twisted, Django, etc.. would prove to be very useful.

Contents:

syntax-case.patch:  Incomplete patch to replace `match.ss` with SYNTAX-CASE

