(module
  (import "env" "print" (func $print (param i32)))
  (import "env" "printLn" (func $printLn (param i32)))
  (global i32 i32.const 1)
  ;; (import "env" "log" (func $log (param i32)))  ;; fails
  (type (func (param i32) (result i32)))
  (func $main
    i32.const 21
    i32.const 21
    i32.add
    call $print)
  (global (mut i32) i32.const 1)
  (type $typeId (func (param $paramId i32) (result i32)))
  (func $f (param $0 i32) (result i32)
    local.get $0
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end)
  (export "main" (func $main)))