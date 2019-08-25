(module
  (func $labelTests
    ;; test return
    loop $block
      return
    end

    ;; test return in nested block
    loop $outerBlock
      loop
        return
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test return
    loop $block (result i64)
      i64.const 1
      return
      ;; polymorphic stack fills in missing operand
      i64.add
    end
    drop

    ;; test return in nested block
    loop $outerBlock (result i64)
      i64.const 1
      loop (result i64)
        i64.const 1
        return
        ;; polymorphic stack
        i64.add
      end
      i64.add
    end
  )
)