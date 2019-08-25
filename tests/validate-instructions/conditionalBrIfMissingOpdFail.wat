(module
  (func $missingOpd
    i32.const 1
    if $block (result i64)
      i64.const 1
      i32.const 0
      br_if $block
      nop
    else
      ;; expected operand missing
      i32.const 0
      br_if $block
      nop
    end
    drop
  )
)
