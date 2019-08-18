(module
  (func $leftoverOpd
    ;; operand left on stack
    loop $block
      i64.const 1
      return
      i64.const 1
    end
    drop
  )
)
