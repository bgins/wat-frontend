(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    i32.const 1
    if
      i32.const 1
      call $print
    else
      nop
    end)
  (export "main" (func $main)))