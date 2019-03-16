(module
  (func $main
    i32.const 21
    i32.const 21
    i32.add
    call $print)
  (global (mut i32) i32.const 1)
  (import "env" "print" (func $print (param i32)))
  (export "main" (func $main)))