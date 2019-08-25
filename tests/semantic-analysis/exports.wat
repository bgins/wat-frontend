(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (export "add" (func $add))

  (global $globalVal (mut i32) i32.const 1)
  (export "result" (global $globalVal))
)