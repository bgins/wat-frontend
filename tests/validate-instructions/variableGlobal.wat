(module
  (global i32 i32.const 1)
  (global (mut i64) i64.const 1)

  (func $getGlobalConst (result i32)
    global.get 0
  )

  (func $getGlobalMut (result i64)
    global.get 1
  )

  (func $globalSet 
    i64.const 1
    global.set 1
  )
)
