(module
  ;; type mismatch
  (func $localset
    (local $0 i64)
    i32.const 1
    local.set 0
  )
)