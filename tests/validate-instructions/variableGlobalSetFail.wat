(module
  (global i32 i32.const 1)

  ;; cannot set global const
  (func $globalset 
    i32.const 1
    global.set 0
  )
)
