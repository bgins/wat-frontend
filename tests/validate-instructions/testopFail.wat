(module
  ;; type mismatch
  (func $eqz32 (param i32) (result i32)
    i64.const 1
    i32.eqz
  )
)
