(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    block
      i32.const 1
      local.set $0
      local.get $0
      call $print
    end)
  (export "main" (func $main)))