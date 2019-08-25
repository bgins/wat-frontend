(module
  ;; func reference does not exist
  (export "main" (func $none))

  ;; global reference does not exist
  (export "result" (global $globalVal))

  (func $sub (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.sub
  )
  (export "sub" (func $sub))

  ;; exports must have a unique name
  (export "sub" (func $sub))
)