(module
  ;; type mistmatch
  (func $eqOne (param i32) (result i32)
    local.get 0
    i64.const 1
    i32.eq
  )
)
