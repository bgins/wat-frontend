(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    i32.const 2
    i32.const 1
    i32.gt_s
    call $print)
  (export "main" (func $main)))