(module
  (import "env" "print" (func $print (param i32)))
  (func $add (param $0 i32) (param $1 i32) (result i32)
    local.get $0
    local.get $1
    i32.add)
  (func $main
    (local $0 i32)
    i32.const 1
    i32.const 2
    call $add
    local.set $0
    local.get $0
    call $print)
  (export "main" (func $main)))