(module
  ;; type mismatch
  (func $extend (param i32) (result i64)
    i64.const 1
    i64.extend_i32_u
  )
)
