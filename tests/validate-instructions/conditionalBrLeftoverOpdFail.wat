(module
  (func $leftoverOpd
    i32.const 1
    if $block
      br $block
      nop
    else
      br $block
      ;; operand left on stack
      i64.const 1
    end
    drop
  )
)