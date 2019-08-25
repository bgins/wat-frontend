(module
  (func $leftoverOpd
    i32.const 1
    if $block
      i32.const 0
      br_if $block
      nop
    else
      ;; operand left on stack
      i64.const 1
      i32.const 0
      br_if $block
      nop
    end
    drop
  )
)