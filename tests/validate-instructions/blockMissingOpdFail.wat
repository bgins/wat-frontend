(module
  (func
    ;; test nested block with no inner result
    block $missing (result i32)
      nop
    end
  )
)