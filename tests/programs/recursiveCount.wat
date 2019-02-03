(module
  (import "env" "print" (func $print (param i32)))
  (func $a (param $0 i32) (result i32)
    local.get $0
    i32.const 9
    i32.lt_s
    if (result i32)
      local.get $0
      call $print
      local.get $0
      i32.const 1
      i32.add
      call $a
    else
      local.get $0
    end)
  (func $main
    block
      i32.const 1
      call $a
      call $print
    end)
  (export "main" (func $main)))