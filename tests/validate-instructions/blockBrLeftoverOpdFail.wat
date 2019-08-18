(module
  (func $leftoverOpd
    ;; operand left on stack
    block $block
      i64.const 1
      br $block
      i64.const 1
    end
    drop
  )
)
