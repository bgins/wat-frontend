(module
  (func $addOne (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.add
  )

  ;; call when no operands on stack
  (func $main
    call $addOne
  )
)
