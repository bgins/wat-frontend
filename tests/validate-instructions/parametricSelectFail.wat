(module
  ;; type mismatch
  (func $select (param i32) (param i64)
    i32.const 1
    local.get 0
    local.get 1
    select
    drop
  )
)