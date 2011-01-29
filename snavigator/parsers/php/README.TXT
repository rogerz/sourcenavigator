This directory contains the source code for a PHP browser
for Source-Navigator. The PHP browser is a fuzzy source
code parser, just like the other parsers included in
Source-Navigator. PHP source code need not be syntactically
perfect in order to be parsed by the PHP browser because
it searches for fragments of recognizable code. It may,
however, occasionally barf on assertions that make it
fruitless to continue parsing a file.  By default, the
files with the extension '.php' (or patterns like '.php*')
will be processed by this browser. The supported language
constructs are as follows:

Mode Switching: Use "<?" to switch to PHP mode and "?> to
                switch back to HTML mode. The "<?php" and
                "<?=" PHP mode entry syntax is also supported.

Comments: // # and /**/ style comments are supported.
          Comments are highlighted in the editor.

Variables: Global, super global, instance/class variables,
           and local variables are supported. Cross
           referencing works for scalar variables and array
           variables. Simple variables within double quoted
           string and heredoc strings are also supported.
           Support for object dereferencing of class member
           variables is available.

Functions: Functions are fully supported. Nested functions
           are also supported. Class member functions using
           object derefencing with '->' and '::' is now
           available with the PHP class support. Functions
           are highlighted inside the editor.

Include: The keywords include, include_once, require, and 
         require_once are recognized as include relationship
         constructs. These include relationships can be
         viewed using the include browser in Source-Navigator.
         Note that using these keywords with variables
         will not produce the desired result.

Keywords: Keywords defined in the keyword appendix of the
          PHP manual are, for the most part, recognized and
          highlighted in the editor.

Classes: Support for PHPv4 and PHPv5 classes are implemented.
         There is support for cross references which use the
	 reserved variables '$this' and '$parent', and also
	 class static functions and properties that use the
	 patterns:
		'classname::funcname($args)'
		'classname::propname'
		'self::funcname($args)'
		'self::propname'
		'parent::funcname($args)'
		'parent::propname'


Note to developers:

A test suite that covers most of the supported PHP language
constructs is located in the file tests/parsers/php.test.
This test script must be run inside the hyper shell via
the "source" Tcl command with the phpbrowser executable
available on the PATH.

