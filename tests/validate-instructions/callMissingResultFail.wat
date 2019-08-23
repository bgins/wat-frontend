(module
  ;; operand missing on return
  (func $one (result i32)
    nop
  )

  (func $main
    call $one
  )
)
