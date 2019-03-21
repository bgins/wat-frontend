# wat-frontend

`wat-frontend` is a compiler frontend for WebAssembly text.

`wat-frontend` should be considered a work in progress. Many parts of the
WebAssembly specification have not been implemented. The issues on this
repository should give an idea of which parts are missing.

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

This will produce a `wat-frontend` binary in `dist/build/wat-frontend/`.

## Use

The lexer and parser target WebAssembly text files (`.wat`). The lexer can also
be used on files with WebAssembly text tokens (`.tok`).

Assuming a directory that contains the `wat-frontend` binary, `identifier.tok`,
and `add.wat`, you can use the lexer to print token streams with the following commands:
```
$ cat identifier.tok 
$name
$ ./wat-frontend lex identifier.tok 
• Token stream for identifier.tok •
[$name]
$ cat add.wat 
(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    i32.const 21
    i32.const 21
    i32.add
    call $print)
  (export "main" (func $main)))
$ ./wat-frontend lex add.wat 
• Token stream for add.wat •
['(',module,'(',import,"env","print",'(',func,$print,'(',param,i32,')',')',')','(',func,$main,i32.const,21,i32.const,21,i32.add,call,$print,')','(',export,"main",'(',func,$main,')',')',')']
$ 
```

The following command uses the parser to print an AST for `add.wat`:
```
$ ./wat-frontend parse add.wat 
• AST for add.wat •
  module
    import "env" "print"
      func $print
        typeuse
          param i32
    func $main
      typeuse
      i32.const 21
      i32.const 21
      i32.add
      call $print
    export "main"
      func $main
$ 
```

The `-o` flag writes the output of the lexer or parser to file in a target
directory:
```
$ ./wat-frontend parse add.wat -o output/
$ cat output/add.ast.out 
module
  import "env" "print"
    func $print
      typeuse
        param i32
  func $main
    typeuse
    i32.const 21
    i32.const 21
    i32.add
    call $print
  export "main"
    func $main
$ 
```

The target directory must already exist. An `.ast.out` file is produced on
success and a `.ast.err` file is produced on failure. The output from the lexer
follows a similar pattern, but with `ast` replaced by `toks`.


The `-h` flag prints a summary of command line options:
```
$ ./wat-frontend -h
wat-frontend - a compiler frontend for WebAssembly text

Usage: wat-frontend PHASE TARGET [-o|--out DIRECTORY]
  Lexes and parses WebAssembly text or tokens

Available options:
  PHASE                    lex [produce a token stream], parse [produce an AST
                           directly from source], check [*experimental* perform
                           semantic analysis on an AST]
  TARGET                   A target .wat or .tok file
  -o,--out DIRECTORY       Optional output directory. Print to console if
                           missing.
  -h,--help                Show this help text
$ 
```

`wat-frontend` can also be run with `cabal run`:
```
$ cabal run -- parse tests/programs/add.wat 
Preprocessing executable 'wat-frontend' for wat-frontend-0.1.0.0...
Running wat-frontend...
• AST for add.wat •
  module
    import "env" "print"
      func $print
        typeuse
          param i32
    func $main
      typeuse
      i32.const 21
      i32.const 21
      i32.add
      call $print
    export "main"
      func $main
$ 
```

## Tests

Tests are kept in the following subdirectories of `tests`: 

- `tokens`: plain tokens with the `.tok` extension, lexer tests
- `components`: modules with one type of component, parser tests (some
  of these tests are not valid modules)
- `programs`: full programs for testing any phase
- `limits`: tests of integer bounds and representation

`dotest` is a regression test script meant to target one of the above
subdirectories. On first run, it will produce expected results if they are not
present. On subsequent runs, it will `diff` the output with the expected results
and print any differences to the console. An empty expected file means the
result is not expected.

The script will lex or parse all tests in a single directory. For example:
```
./dotest parse tests/programs/
```

This command will parse all tests in `tests/programs/` and output the results to
`tests/programs/parse/`. The output directory name matches the name of the
phase run over the tests.

## Validation

`LexSpec.hs` is a token fuzzing suite. You can run it with:
```
cabal test validation
```
The test generates arbitrary tokens, untokenizes them to a space-separated
string, lexes the string, and checks that the resulting tokens are the same as
the original tokens.
