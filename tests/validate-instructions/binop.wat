(module
  (func $addOne (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.add
  )

  (func $subOne (param i64) (result i64)
    local.get 0
    i64.const 1
    i64.sub
  )

  (func $min (param f32) (param f32) (result f32)
    local.get 0
    local.get 1
    f32.min
  )

  (func $max (param f64) (param f64) (result f64)
    local.get 0
    local.get 1
    f64.max
  )
)