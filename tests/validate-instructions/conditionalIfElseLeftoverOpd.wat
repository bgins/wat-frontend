(module
  (func
    ;; leftover operand not expected
    i32.const 1
    if $ifConst
      i32.const 2
    else
      i32.const 3
    end 
    drop
  )
)
