toml-parser
===========

[![Hackage](https://img.shields.io/hackage/v/toml-parser.svg)](https://hackage.haskell.org/package/toml-parser) [![Build Status](https://secure.travis-ci.org/glguy/toml-parser.png?branch=master)](http://travis-ci.org/glguy/toml-parser)

This package provides a complete and light-weight parser for the TOML
configuration language. TOML is specified by <https://github.com/toml-lang/toml>.
This language is designed to be easy to understand and unambiguous.

This implementation uses Alex and Happy to generate an efficient lexer
and parser. The code is fully commented and should be useful as an example
of how to use these tools to parse other languages.

Currently the package provides a consistent view of the reified declaration
information about datatypes, newtypes, data families, and newtype families.
These interfaces abstract away the differences in the normal and GADT syntax
used to define these types.

Example
-------

```ini
# This is a TOML document.

title = "Example document"

[section]
string    = "hello, world"
integers  = [0, +1, -2, 3_333_333]
float     = 4.2e10
inlinemap = { x = 1, y = 'two' }

[nesting]

  [nesting.first]
  longstring = """multiline "string" syntax"""
  longquote  = '''escapes \not \interpreted'''

  [nesting.second]
  datetime    = 2017-05-04T12:13:14Z
  lookinggood = true
```

Contact Information
-------------------

Please contact me via GitHub or on the #haskell IRC channel on irc.freenode.net
