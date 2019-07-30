(module
  (func $extend (param i32) (result i64)
    local.get 0
    i64.extend_i32_u
  )

  (func $reinterpret32 (param i32) (result f32)
    local.get 0
    f32.reinterpret_i32
  )

  (func $convertI32_F64 (param i32) (result f64)
    local.get 0
    f64.convert_i32_u
  )

  (func $wrap (param i64) (result i32)
    local.get 0
    i32.wrap_i64
  )

  (func $convertI64_F64 (param i64) (result f32)
    local.get 0
    f32.convert_i64_u
  )

  (func $reinterpet64 (param i64) (result f64)
    local.get 0
    f64.reinterpret_i64
  )

  (func $truncF32_I32 (param f32) (result i32)
    local.get 0
    i32.trunc_f32_s
  )

  (func $truncF32_I64 (param f32) (result i64)
    local.get 0
    i64.trunc_f32_u
  )

  (func $promote (param f32) (result f64)
    local.get 0
    f64.promote_f32
  )

  (func $truncF64_I32 (param f64) (result i32)
    local.get 0
    i32.trunc_f64_s
  )

  (func $truncF64_I64 (param f64) (result i64)
    local.get 0
    i64.trunc_f64_s
  )

  (func $demote (param f64) (result f32)
    local.get 0
    f32.demote_f64
  )

)