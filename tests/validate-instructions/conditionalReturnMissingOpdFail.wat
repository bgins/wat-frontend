(module
  (func $missingOpd (result i64)
    i32.const 1
    if $block
      i64.const 1
      return
      nop
    else
      ;; expected operand missing
      return
      nop
    end
    drop
  )
)
