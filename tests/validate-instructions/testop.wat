(module
  (func $eqz32 (param i32) (result i32)
    local.get 0
    i32.eqz
  )

  (func $eqz64 (param i64) (result i32)
    local.get 0
    i64.eqz
  )
)