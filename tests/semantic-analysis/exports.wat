(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func $sub (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.sub
  )
  (export "add" (func $add))
  (export "sub" (func $sub))
)