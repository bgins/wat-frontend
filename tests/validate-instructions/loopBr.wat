;; here be infinite loops
(module
  (func $labelTests
    ;; test br
    loop $block
      br $block
      nop
    end

    ;; test br to enclosing block
    loop $outerBlock
      loop
        br $outerBlock
        nop
      end
      nop
    end

    ;; test br to enclosing func
    loop
      loop
        br $labelTests
        nop
      end
      nop
    end
  )

  (func $indexTests
    ;; test br
    loop
      br 0
      nop
    end

    ;; test br to enclosing block
    loop
      loop
        br 1
        nop
      end
      nop
    end

    ;; test br to enclosing func
    loop 
      loop
        br 2
        nop
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test br
    loop $block (result i64)
      i64.const 1
      br $block
      ;; polymorphic stack fills in missing operand
      i64.add
    end
    drop

    ;; test br to enclosing block
    loop $outerBlock (result i64)
      i64.const 1
      loop (result i64)
        i64.const 1
        br $outerBlock
        ;; polymorphic stack
        i64.add
      end
      i64.add
    end
    drop

    ;; test br to enclosing func
    loop $outerBlock (result i64)
      loop (result i64)
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