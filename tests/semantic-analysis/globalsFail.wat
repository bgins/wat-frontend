(module
  ;; type mismatch
  (global (mut i32)
    i64.const 1
    i64.const 1
    i64.add
  )
)