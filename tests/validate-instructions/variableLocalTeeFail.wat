(module
  ;; type mismatch
  (func $localtee (result i32)
    (local $0 i32)
    i64.const 1
    local.tee 0
  )
)
