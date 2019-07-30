(module
  (func $clz (param i32) (result i32)
    local.get 0
    i32.clz
  )

  (func $popcnt (param i64) (result i64)
    local.get 0
    i64.popcnt
  )

  (func $abs (param f32) (result f32)
    local.get 0
    f32.abs
  )

  (func $ceil (param f64) (result f64)
    local.get 0
    f64.ceil
  )
)