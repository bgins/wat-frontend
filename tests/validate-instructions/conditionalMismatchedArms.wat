(module
  (func
    ;; test if-else leaves result
    i32.const 1
    if $ifElseConst (result i32)
      i32.const 1
    else
      i64.const 0
    end
    drop
  )
)
