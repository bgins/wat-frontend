(module
  (import "env" "print" (func $print (param i32)))
  (func $fib (param $0 i32) (result i32)
    local.get $0
    i32.const 1
    i32.le_s
    if (result i32)
      local.get $0
    else
      local.get $0
      i32.const 1
      i32.sub
      call $fib
      local.get $0
      i32.const 2
      i32.sub
      call $fib
      i32.add
    end)
  (func $main
    block
      i32.const 45
      call $fib
      call $print
    end)
  (export "main" (func $main)))