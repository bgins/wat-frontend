(module
  (func $labelTests
    ;; test return
    i32.const 1
    if $block
      return
    end

    ;; test return in nested block
    i32.const 1
    if $outerBlock
      i32.const 1
      if
        return
      else 
        return
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test return
    i32.const 1
    if $block (result i64)
      i64.const 1
      return
      ;; polymorphic stack fills in missing operand
      i64.add
    end
    drop

    ;; test return in nested block
    i32.const 1
    if $outerBlock (result i64)
      i64.const 1
      i32.const 1
      if (result i64)
        i64.const 1
        return
      else
        i64.const 1
        return
        ;; polymorphic stack
        i64.add
      end
      i64.add
    end
  )
)