(module
  (func $missingOpd
    ;; expected operand missing
    loop $block (result i64)
      i32.const 0
      br_if $block
      nop
    end
    drop
  )
)
