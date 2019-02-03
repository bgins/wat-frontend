(module
  (import "env" "print" (func $print (param i32)))
  (func $f (param $0 i32) (result i32)
    local.get $0
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end)
  (func $main
    block
      i32.const 1
      call $f
      call $print
    end)
  (export "main" (func $main)))