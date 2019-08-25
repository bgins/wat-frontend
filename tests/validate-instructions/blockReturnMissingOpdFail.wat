(module
  (func $missingOpd (result i64)
    ;; expected operand missing
    block $block
      return
      nop
    end
    drop
  )
)
