(module
  ;; expect i32, but result is i64
  (func $constI32 (result i32)
    i64.const 1
  )
)
