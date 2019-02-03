# wat-frontend

A compiler frontend for WebAssembly text.

## Build

This project uses `Parsec` and builds with `ghc`.

```
ghc --make Main.hs
```

## Use

The lexer or parser are run over all tests in a directory. For example:

```
./Main lex tests/tokens/
./Main parse tests/components/
```

The `tokens` directory contains lexical analysis unit tests that are not
suitable for parsing. The `components` directory contains tests of one or two
components and empty modules. The `programs` directory contains full programs.
(These will not parse at the moment).

The parser and lexer produce output or error files alongside the tests
indicating success or failure. In some cases, failure is the intended result as
indicated by the test name.
