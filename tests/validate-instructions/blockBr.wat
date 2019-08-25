(module
  (func $labelTests
    ;; test br
    block $block
      br $block
      nop
    end

    ;; test br to enclosing block
    block $outerBlock
      block
        br $outerBlock
        nop
      end
      nop
    end

    ;; test br to enclosing func
    block
      block
        br $labelTests
        nop
      end
      nop
    end
  )

  (func $indexTests
    ;; test br
    block
      br 0
      nop
    end

    ;; test br to enclosing block
    block
      block
        br 1
        nop
      end
      nop
    end

    ;; test br to enclosing func
    block 
      block
        br 2
        nop
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test br
    block $block (result i64)
      i64.const 1
      br $block
      ;; polymorphic stack fills in missing operand
      i64.add
    end
    drop

    ;; test br to enclosing block
    block $outerBlock (result i64)
      i64.const 1
      block (result i64)
        i64.const 1
        br $outerBlock
        ;; polymorphic stack
        i64.add
      end
      i64.add
    end
    drop

    ;; test br to enclosing func
    block $outerBlock (result i64)
      block (result i64)
        i64.const 1
        br $resultsMatch
        ;; polymorphic stack
        i64.add
      end
      br 1
      i64.const 1
      i64.add
    end
    i64.const 1
    i64.add
  )
)