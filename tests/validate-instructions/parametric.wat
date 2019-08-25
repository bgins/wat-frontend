(module
  (func $drop (param i32)
    local.get 0
    drop
  )

  (func $select (param i64) (param i64)
    i32.const 1
    local.get 0
    local.get 1
    select
    drop
  )
)