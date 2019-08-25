(module
  (func $missingOpd
    ;; expected operand missing
    block $block (result i64)
      br $block
      nop
    end
    drop
  )
)
