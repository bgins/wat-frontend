(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    block
      i32.const 0
      local.set $0
      block
        loop
          i32.const 1
          local.get $0
          i32.const 9
          i32.lt_s
          i32.sub
          br_if 1
          local.get $0
          i32.const 1
          i32.add
          local.set $0
          local.get $0
          call $print
          br 0
        end
      end
    end)
  (export "main" (func $main)))