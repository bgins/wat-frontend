(module
  (import "env" "print" (func $print (param i32)))
  (func $a (param $0 i32) (result i32)
    local.get $0
    i32.const 1
    i32.add)
  (func $b (param $0 i32) (result i32)
    local.get $0
    call $a
    i32.const 1
    i32.add)
  (func $c (param $0 i32) (result i32)
    local.get $0
    call $b
    i32.const 1
    i32.add)
  (func $d (param $0 i32) (result i32)
    local.get $0
    call $c
    i32.const 1
    i32.add)
  (func $main
    (local $0 i32)
    block
      i32.const 0
      call $d
      local.set $0
      local.get $0
      call $print
    end)
  (export "main" (func $main)))