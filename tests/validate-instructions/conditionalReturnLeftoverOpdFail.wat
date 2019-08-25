(module
  (func $leftoverOpd
    i32.const 1
    if $block
      return
      nop
    else
      return
      ;; operand left on stack
      i64.const 1
    end
    drop
  )
)
