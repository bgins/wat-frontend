(module
  (func
    ;; missing result
    i32.const 1
    if $ifConst (result i32)
      nop
    else
      nop
    end 
    drop
  )
)
