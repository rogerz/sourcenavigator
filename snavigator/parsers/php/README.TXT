This directory contains the source code for a PHP browser
for Source-Navigator. The PHP browser is a fuzzy source
code parser, just like the other parsers include in
Source-Navigator. PHP source code need not be syntactically
perfect in order to be parsed by the PHP browser.
Files with the .php extension will be processed by the
PHP browser. The supported language constructs are as
follows:

Mode Switching: Use "<?" to switch to PHP mode and "?> to
                switch back to HTML mode. The "<?php" and
                "<?=" PHP mode entry syntax is also supported.

Comments: // # and /**/ style comments are supported.
          Comments are highlighted in the editor.

Variables: Both global variables and local variables are
           supported. Cross referencing works for scalar
           variables and array variables. Simple variables
           within double quoted string and heredoc strings
           are also supported.

Functions: Functions are supported, but functions defined
           in other functions are not. Functions are
           highlighted in the editor.

Include: The keywords include, include_once, require, and 
         require_once are recognized as include relationship
         constructs. These include relationships can be
         viewed using the include browser in Source-Navigator.

Keywords: Each of the keywords defined in appendix G of
          the PHP manual are recognized and highlighted
          in the editor.

Classes: Support for classes is not yet implemented.


Note to developers:

A test suite that covers each of the supported PHP language
constructs is located in the file tests/parsers/php.test.
This test script must be run inside the hyper shell via
the "source" Tcl command with the phpbrowser executable
available on the PATH.

