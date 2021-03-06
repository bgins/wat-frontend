(module
  (func
    ;; test if that does nothing
    i32.const 1
    if $ifNop
      nop
    end 

    ;; test if leaves result
    i32.const 1
    if $ifConst (result i32)
      i32.const 2
    end 
    drop

    ;; test nested if leaves result
    i32.const 1
    if $ifConst (result i32)
      i32.const 1
      if $ifInnerConst (result i32)
        i32.const 2
      end
    end 
    drop

    ;; test if-else leaves result
    i32.const 1
    if $ifElseNop        
      nop
    else
      nop
    end

    ;; test if-else leaves result
    i32.const 1
    if $ifElseConst (result i32)
      i32.const 1
    else
      i32.const 0
    end
    drop

    ;; test nested if leaves result in if-else
    i32.const 1
    if $ifElseConst (result i32)
      i32.const 1
      if $ifNestedInIf (result i32)
        i32.const 2
      end
    else
      i32.const 1
      if $ifNestedInElse (result i32)
        i32.const 3
      end
    end
    drop

    ;; test nested if leaves result in if-else
    i32.const 1
    if $ifElseConst (result i32)
      i32.const 1
      if $ifelseNestedInIf (result i32)
        i32.const 2
      else
        i32.const 3
      end
    else
      i32.const 1
      if $ifelseNestedInElse (result i32)
        i32.const 4
      else
        i32.const 5
      end
    end
    drop

  )
)
