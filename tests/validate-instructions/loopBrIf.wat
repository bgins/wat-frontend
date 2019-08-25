(module
  (func $labelTests
    ;; test br_if
    loop $block
      i32.const 1
      br_if $block
      nop
    end

    ;; test br_if to enclosing loop
    loop $outerBlock
      loop
        i32.const 1
        br_if $outerBlock
        nop
      end
      nop
    end

    ;; test br_if to enclosing func
    loop
      loop
        i32.const 1
        br_if $labelTests
        nop
      end
      nop
    end
  )

  (func $indexTests
    ;; test br_if
    loop
      i32.const 1
      br_if 0
      nop
    end

    ;; test br_if to enclosing block
    loop
      loop
        i32.const 1
        br_if 1
        nop
      end
      nop
    end

    ;; test br_if to enclosing func
    loop 
      loop
        i32.const 1
        br_if 2
        nop
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test br_if
    loop $block (result i64)
      i64.const 1
      i32.const 0
      br_if $block
      i64.const 1
      i64.add
    end
    drop

    ;; test br_if to enclosing block
    loop $outerBlock (result i64)
      i64.const 1
      loop (result i64)
        i64.const 1
        i32.const 1
        br_if $outerBlock
        i64.const 1
        i64.add
      end
      i64.add
    end
    drop

    ;; test br_if to enclosing func
    loop $outerBlock (result i64)
      loop (result i64)
        i64.const 1
        i32.const 1
        br_if $resultsMatch
        i64.const 1
        i64.add
      end
      i32.const 0
      br_if 1
      i64.const 1
      i64.add
    end
    i64.const 1
    i64.add
  )
)
