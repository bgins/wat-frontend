(module
  (func $eqOne (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.eq
  )

  (func $neOne (param i64) (result i32)
    local.get 0
    i64.const 1
    i64.ne
  )

  (func $lt (param f32) (param f32) (result i32)
    local.get 0
    local.get 1
    f32.lt
  )

  (func $gt (param f64) (param f64) (result i32)
    local.get 0
    local.get 1
    f64.gt
  )
)