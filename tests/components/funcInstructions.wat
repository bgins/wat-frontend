(module
  (func $funcId (type 1) (param i32) (result i32)
  (local $localId i32)
  ;; invalid program, only a torture test of instructions

  ;; block
  ;; loop
  ;; if .. else .. end
  unreachable
  nop
  br 1
  br $labelId 
  br_if 1
  br_if $lableId 
  ;; br_table
  return
  call 1
  call $funcId
  call_indirect (type 1)
  call_indirect (type 1) (param i32) (result i32)
  drop
  select
  local.get 1
  local.get $localId 
  local.set 1
  local.set $localId
  local.tee 1
  local.tee $localId
  global.get 1
  global.get $globalId
  global.set 1
  global.set $globalId

  ;; memory instructions
  
  i32.const 1
  i32.const +1
  i32.const -1
  i64.const 1
  i64.const +1
  i64.const -1
  ;; f32.const
  ;; f64.const
  i32.clz
  i32.ctz
  i32.popcnt
  i32.add
  i32.sub
  i32.mul
  i32.div_s
  i32.div_u
  i32.rem_s
  i32.rem_u
  i32.and
  i32.or
  i32.xor
  i32.shl
  i32.shr_s
  i32.shr_u
  i32.rotl
  i32.rotr
  i64.clz
  i64.ctz
  i64.popcnt
  i64.add
  i64.sub
  i64.mul
  i64.div_s
  i64.div_u
  i64.rem_s
  i64.rem_u
  i64.and
  i64.or
  i64.xor
  i64.shl
  i64.shr_s
  i64.shr_u
  i64.rotl
  i64.rotr
  f32.abs
  f32.neg
  f32.ceil
  f32.floor
  f32.trunc
  f32.nearest
  f32.sqrt
  f32.add
  f32.sub
  f32.mul
  f32.div
  f32.min
  f32.max
  f32.copysign
  f64.abs
  f64.neg
  f64.ceil
  f64.floor
  f64.trunc
  f64.nearest
  f64.sqrt
  f64.add
  f64.sub
  f64.mul
  f64.div
  f64.min
  f64.max
  f64.copysign
  i32.eqz
  i32.eq
  i32.ne
  i32.lt_s
  i32.lt_u
  i32.gt_s
  i32.gt_u
  i32.le_s
  i32.le_u
  i32.ge_s
  i32.ge_u
  i64.eqz
  i64.eq
  i64.ne
  i64.lt_s
  i64.lt_u
  i64.gt_s
  i64.gt_u
  i64.le_s
  i64.le_u
  i64.ge_s
  i64.ge_u
  f32.eq
  f32.ne
  f32.lt
  f32.gt
  f32.le
  f32.ge
  f64.eq
  f64.ne
  f64.lt
  f64.gt
  f64.le
  f64.ge
  i32.wrap_i64
  i32.trunc_f32_s
  i32.trunc_f32_u
  i32.trunc_f64_s
  i32.trunc_f64_u
  i64.extend_i32_s
  i64.extend_i32_u
  i64.trunc_f32_s
  i64.trunc_f32_u
  i64.trunc_f64_s
  i64.trunc_f64_u
  f32.convert_i32_s
  f32.convert_i32_u
  f32.convert_i64_s
  f32.convert_i64_u
  f32.demote_f64
  f64.convert_i32_s
  f64.convert_i32_u
  f64.convert_i64_s
  f64.convert_i64_u
  f64.promote_f32
  i32.reinterpret_f32
  i64.reinterpret_f64
  f32.reinterpret_i32
  f64.reinterpret_i64
  )
)