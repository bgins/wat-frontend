(module
  (func $localget (result i32)
    (local $0 i32)
    local.get 0
  )

  (func $localset
    (local $0 i64)
    i64.const 1
    local.set 0
  )

  (func $localtee (result i32)
    (local $0 i32)
    i32.const 1
    local.tee 0
  )
)