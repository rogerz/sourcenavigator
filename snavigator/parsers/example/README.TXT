This directory contains the source code for a simple
little example language. This browser provides an
basic example of how a new parser for Source-Navigator
can be constructed.

This example language supports global and local variables,
function declarations, function invocations, the keywords
global and reture, integer values, string values,
comments, and xrefs for function and variables.


Supported Language Features:

foo = 1;
bar = "hello";

/* This is a great function */
declare myfunc() {
    global foo;

    name = "bob";

    return foo;
}

ret_val = myfunc();



Note to developers:

A test suite that covers each of the supported language
constructs is located in the file tests/parsers/example.test.
This test script must be run inside the hyper shell via
the "source" Tcl command with the exlbrowser executable
available on the PATH.

