(module
  (global i32 i32.const 1)
  (global (mut i64) i32.const 1)

  (func $globalget (result i32)
    global.get 0
  )

  (func $globalget (result i64)
    global.get 1
  )

  (func $globalset 
    i64.const 1
    global.set 1
  )
)

