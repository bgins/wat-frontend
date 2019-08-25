(module
  ;; type mismatch in the unreachable code
  (func
    unreachable
    i64.const 3
    i32.add
    drop
  )
)

