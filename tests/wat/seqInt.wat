(module
  (import "env" "print" (func $print (param i32)))
  (func $a (param $0 i32) (result i32)
    local.get $0
    i32.const 1
    i32.add
    local.set $0
    local.get $0)
  (func $main
    block
      i32.const 1
      call $a
      call $print
    end)
  (export "main" (func $main)))