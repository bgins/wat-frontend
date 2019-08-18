(module
  (func $leftoverOpd
    ;; operand left on stack
    loop $block
      br $block
      i64.const 1
      nop
    end
    drop
  )
)