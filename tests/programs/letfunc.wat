(module
  (import "env" "print" (func $print (param i32)))
  (func $a (result i32)
    i32.const 1)
  (func $main
    block
      call $a
      call $print
    end)
  (export "main" (func $main)))