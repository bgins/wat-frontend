(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    i32.const 21
    i32.const 21
    i32.sub
    call $print)
  (export "main" (func $main)))