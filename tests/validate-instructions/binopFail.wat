(module
  ;; expect two operands on stack, but only one
  (func $addOne (param i32) (result i32)
    local.get 0
    i32.add
  )
)