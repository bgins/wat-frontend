(module
  (global i32 i32.const 1)

  ;; global not defined
  (func $globalget (result i32)
    global.get 1
  )
)
