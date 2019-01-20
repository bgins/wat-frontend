(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    (local $1 i32)
    block
      i32.const 1
      local.set $0
      i32.const 0
      local.set $1
      local.get $0
      if
        local.get $1
        if
          i32.const 0
          call $print
        else
          i32.const 1
          call $print
        end
      else
        i32.const 0
        call $print
      end
    end)
  (export "main" (func $main)))