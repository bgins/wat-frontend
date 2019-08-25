(module
  (func $leftoverOpd
    ;; operand left on stack
    loop $block
      i64.const 1
      i32.const 0
      br_if $block
      nop
    end
    drop
  )
)