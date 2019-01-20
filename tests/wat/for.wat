(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    (local $1 i32)
    block
      i32.const 1
      local.set $0
      i32.const 9
      local.set $1
      loop
        local.get $0
        call $print
        local.get $0
        i32.const 1
        i32.add
        local.set $0
        local.get $0
        local.get $1
        i32.le_s
        br_if 0
      end
    end)
  (export "main" (func $main)))