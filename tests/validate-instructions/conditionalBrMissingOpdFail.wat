(module
  (func $missingOpd
    i32.const 1
    if $block (result i64)
      i64.const 1
      br $block
      nop
    else
      br $block
      ;; expected operand missing
      nop
    end
    drop
  )
)
