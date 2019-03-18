# wat-frontend

A compiler frontend for WebAssembly text.

## Setup

Initialize a cabal sandbox (recommended):
```
cabal sandbox init
```

Install the project dependencies into the sandbox:
```
cabal install --only-dependencies
```

## Build

Build the project:
```
cabal build
```

A `wat-frontend` binary will be output to `dist/build/wat-frontend/`.

## Use

The lexer, parser and typechecker target WebAssembly text files (`.wat`). The
lexer may also be used on files with WebAssembly text tokens (`.tok`).
For example:
```
./wat-frontend lex identifier.tok
./wat-frontend lex add.wat
./wat-frontend parse add.wat
./wat-frontend parse add.wat -o output/
```

The first two examples print a token stream to the console. The third example
prints an AST to the console. The last command will produce the output file
`add.wat.ast.out` in the `output` directory on success or the error file
`add.wat.ast.err` on failure. The output from the lexer follows a similar
pattern, but with `ast` replaced by `toks`.

`wat-frontend` can also be used with `cabal run`:
```
cabal run -- parse add.wat
```

Run `./wat-frontend -h` for a summary of command line options.

## Tests

Tests are kept in the `tests` directory in a few subdirectories: 

- `tokens`: plain tokens with the `.tok` extension, tests for the lexer
- `components`: modules with one type of component, tests for the parser (some
  of these tests are not be valid modules)
- `programs`: full programs for testing any phase
- `limits`: tests of integer bounds and representation

`dotest` is a regression test script meant to target one of the above
subdirectories. On first run, it will produce expected results if they are not
present. On subsequent runs, it will `diff` the output with the expected results
and print any differences to the console. An empty expected file means the
result is not expected.

The script will lex, parse, or check all tests in a single directory.
```
./dotest parse tests/programs/
```

This command will parse all tests in `tests/programs/` and output the results to
`tests/programs/parse/`. The output directory name will match the name of the
phase run over the tests.

## Validation

`LexSpec.hs` is a token fuzzing suite that can be run with:
```
cabal test validation
```
The test generates arbitrary tokens, untokenizes them to a space-separated
string, lexes the string, and checks that the resulting tokens are the same as
the original tokens.
