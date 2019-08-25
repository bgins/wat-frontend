(module
  (func $labelTests
    ;; test return
    block $block
      return
    end

    ;; test return in nested block
    block $outerBlock
      block
        return
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test return
    block $block (result i64)
      i64.const 1
      return
      ;; polymorphic stack fills in missing operand
      i64.add
    end
    drop

    ;; test return in nested block
    block $outerBlock (result i64)
      i64.const 1
      block (result i64)
        i64.const 1
        return
        ;; polymorphic stack
        i64.add
      end
      i64.add
    end
  )
)