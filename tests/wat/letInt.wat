(module
  (import "env" "print" (func $print (param i32)))
  (func $f (result i32)
    (local $0 i32)
    block (result i32)
      i32.const 1
      local.set $0
      local.get $0
    end)
  (func $main
    block
      call $f
      call $print
    end)
  (export "main" (func $main)))