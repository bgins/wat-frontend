(module
  (import "env" "print" (func $print (param i32)))
  (func $a (result i32)
    i32.const 1)
  (func $b (result i32)
    i32.const 2)
  (func $c (result i32)
    i32.const 3)
  (func $d (result i32)
    i32.const 4)
  (func $main
    block
      call $a
      call $b
      call $c
      call $d
      i32.add
      i32.add
      i32.add
      call $print
    end)
  (export "main" (func $main)))