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
cabal install --only-dependencies --enable-tests
```

## Build

Build the project:
```
cabal build
```

This will produce a `wat-frontend` binary in `dist/build/wat-frontend/`.

## Use

The lexer, parser, and typechecker target WebAssembly text files (`.wat`). The lexer can also
be used on files with WebAssembly text tokens (`.tok`).

Assuming a directory that contains the `wat-frontend` binary and `add.wat`, the
lexer can be used to produce token streams:
```
$ cat add.wat 
(module
  (type $binop (func (param i32) (param i32) (result i32)))
  (func $add (type $binop) (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add)
  (export "add" (func $add)))
$ ./wat-frontend lex add.wat 
• Token stream for add.wat •
['(',module,'(',type,$binop,'(',func,'(',param,i32,')','(',param,i32,')','(',result,i32,')',')',')','(',func,$add,'(',type,$binop,')','(',param,$lhs,i32,')','(',param,$rhs,i32,')','(',result,i32,')',local.get,$lhs,local.get,$rhs,i32.add,')','(',export,"add",'(',func,$add,')',')',')']
$
```

The parser produces ASTs:
```
$ ./wat-frontend parse add.wat 
• AST for add.wat •
  module
    type $binop
      functype
        param i32
        param i32
        result i32
    func $add
      typeuse $binop
        param $lhs i32
        param $rhs i32
        result i32
      local.get $lhs
      local.get $rhs
      i32.add
    export "add"
      func $add
$
```

The typechecker checks modules and prints verbose analysis:
```
$ ./wat-frontend check add.wat 
① Check import order
② Add types to the context
∙context∙
  types
  funcs
  globals
  start
  exports
  valid True

🞅∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
∙component∙
  type $binop
    functype
      param i32
      param i32
      result i32

∙context∙
  types
      $binop : i32 -> i32 -> i32
  funcs
  globals
  start
  exports
  valid True

③ Add imports, funcs, and globals to the context
∙context∙
  types
      $binop : i32 -> i32 -> i32
  funcs
  globals
  start
  exports
  valid True

🞅∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
∙component∙
  func $add
    typeuse $binop
      param $lhs i32
      param $rhs i32
      result i32
    local.get $lhs
    local.get $rhs
    i32.add

∙context∙
  types
      $binop : i32 -> i32 -> i32
  funcs
      $add : {0}
  globals
  start
  exports
  valid True

④ Check func bodies, starts and exports
λ••••••••••••••••
checking func body:
  func $add
    typeuse $binop
      param $lhs i32
      param $rhs i32
      result i32
    local.get $lhs
    local.get $rhs
    i32.add

∙🞎∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
∙local context∙
  locals
      $lhs : i32
      $rhs : i32
  operand stack
      [ ]
  control stack
      0 | label $add
          label types [ i32 ]
          result types [ i32 ]
          entry height 0
          unreachable False


∙⬚∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
  local.get $lhs

  locals
      $lhs : i32
      $rhs : i32
  operand stack
      [ i32 ]


∙⬚∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
  local.get $rhs

  locals
      $lhs : i32
      $rhs : i32
  operand stack
      [ i32 i32 ]


∙⬚∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
  i32.add

  locals
      $lhs : i32
      $rhs : i32
  operand stack
      [ i32 ]


🞎∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
∙local context∙
  locals
      $lhs : i32
      $rhs : i32
  operand stack
      [ i32 ]
  control stack


∙context∙
  types
      $binop : i32 -> i32 -> i32
  funcs
      $add : {0}
  globals
  start
  exports
  valid True

🞅∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙∙
∙component∙
  export "add"
    func $add

⑤ Show final context
∙context∙
  types
      $binop : i32 -> i32 -> i32
  funcs
      $add : {0}
  globals
  start
  exports
       "add"
  valid True

✓ Module is valid
$
```

The `-o` flag writes the output of the lexer or parser to file to a target
directory (the directory must already exist):
```
$ ./wat-frontend parse add.wat -o output/
$ cat output/add.ast.out
module
  type $binop
    functype
      param i32
      param i32
      result i32
  func $add
    typeuse $binop
      param $lhs i32
      param $rhs i32
      result i32
    local.get $lhs
    local.get $rhs
    i32.add
  export "add"
    func $add
$
```

An `.ast.out` file is produced on success and a `.ast.err` file is produced on
failure. The output from the lexer follows a similar pattern, but with `ast`
replaced by `toks`. The typechecker does not currently have an option to write
to file.


The `-h` flag prints a summary of command line options:
```
$ ./wat-frontend -h
wat-frontend - a compiler frontend for WebAssembly text

Usage: wat-frontend PHASE TARGET [-o|--out DIRECTORY]
  Lexes and parses WebAssembly text or tokens

Available options:
  PHASE                    lex [produce a token stream], parse [produce an AST],
                           check [perform semantic analysis]
  TARGET                   A target .wat or .tok file
  -o,--out DIRECTORY       Optional output directory. Print to console if
                           missing. Not implemented for check.
  -h,--help                Show this help text
$
```

`wat-frontend` can also be run using `cabal run`:
```
$ cabal run -- parse add.wat 
Preprocessing executable 'wat-frontend' for wat-frontend-0.1.0.0..
Building executable 'wat-frontend' for wat-frontend-0.1.0.0..
Running wat-frontend...
• AST for add.wat •
  module
    type $binop
      functype
        param i32
        param i32
        result i32
    func $add
      typeuse $binop
        param $lhs i32
        param $rhs i32
        result i32
      local.get $lhs
      local.get $rhs
      i32.add
    export "add"
      func $add
$
```

## Tests

Tests are kept in the following subdirectories of `tests`:

- `tokens`: plain tokens with the `.tok` extension, lexer tests
- `components`: modules with one type of component, parser tests (some
  of these tests are not valid modules)
- `programs`: full programs for testing any phase
- `limits`: tests of integer bounds and representation
- `semantic-analysis`: tests of modules and components
- `validate-instructions`: tests of instructions

`dotest` is a regression test script meant to target one of the above
subdirectories. On first run, it will produce expected results if they are not
present. On subsequent runs, it will `diff` the output with the expected results
and print any differences. An empty expected file means the result is not
expected.

`dotest` will lex or parse all tests in a single directory. For example:
```
./dotest parse tests/programs/
```

This command will parse all tests in `tests/programs/` and output the results to
`tests/programs/parse/`. The output directory name matches the name of the
phase run over the tests.

At the moment, `docheck` does not test the typechecker.

## Validation

`LexSpec.hs` is a token fuzzing suite. You can run it with:
```
cabal test validation
```
The test generates arbitrary tokens, untokenizes them to a space-separated
string, lexes the string, and checks that the resulting tokens are the same as
the original tokens.
