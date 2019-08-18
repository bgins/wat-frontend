(module
  (func $missingOpd (result i64)
    ;; expected operand missing
    loop $block
      return
      nop
    end
    drop
  )
)
