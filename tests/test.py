from __future__ import with_statement
from os         import system
from StringIO   import StringIO
from contextlib import contextmanager

import sys

class _boa:
    __slots__ = ('stdout', '_old_stdout', 'mod')

    def check_stdout(self, s):
        actual_output = self.stdout.getvalue()
        assert actual_output == s, \
            "Expected ``%s'', but got ``%s''" % (s, actual_output)

@contextmanager
def boa(filename):
    assert 0 == system(
        'mzscheme -r compile.ss "tests/%s.boa" "tests/%s.pyc"' % \
            (filename, filename)), "mzscheme failed"
    b = _boa()
    b._old_stdout = sys.stdout
    sys.stdout = b.stdout = StringIO()
    try:
        b.mod = __import__("tests.%s" % filename)
        yield b
    finally:
        b.stdout.close()
        sys.stdout = b._old_stdout

## There is a ``test_foo'' function for each foo.boa
## They all use the ``boa'' contextmanager, above, using
## Python2.5's ``with'' statement.

def test_print():
    with boa("print") as b:
        b.check_stdout("Hello World\nFun World\n")
    
def test_if():
    with boa("if") as b:
        b.check_stdout("yes\nfoo bar Else stmt\n")
