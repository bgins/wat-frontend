(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    i32.const 1
    call $print
    i32.const 2
    call $print
    i32.const 3
    call $print
    i32.const 4
    call $print
    i32.const 5
    call $print)
  (export "main" (func $main)))