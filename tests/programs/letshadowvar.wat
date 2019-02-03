(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    (local $1 i32)
    block
      i32.const 1
      local.set $0
      block
        i32.const 2
        local.set $1
        local.get $1
        call $print
      end
      local.get $0
      call $print
    end)
  (export "main" (func $main)))