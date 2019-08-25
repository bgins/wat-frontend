(module
  (func $labelTests
    ;; test br
    i32.const 1
    if $block
      br $block
      nop
    else
      br $block
      nop
    end

    ;; test br to enclosing loop
    i32.const 1
    if $outerBlock
      i32.const 1
      if
        br $outerBlock
        nop
      else
        br $outerBlock
        nop
      end
      nop
    end

    ;; test br to enclosing func
    i32.const 1
    if
      i32.const 1
      if
        br $labelTests
        nop
      else
        br $labelTests
        nop
      end
      nop
    end
  )

  (func $indexTests
    ;; test br_if
    i32.const 1
    if
      br 0
      nop
    else
      br 0
      nop
    end

    ;; test br to enclosing block
    i32.const 1
    if
      i32.const 1
      if
        br 1
        nop
      else
        br 1
        nop
      end
      nop
    end

    ;; test br to enclosing func
    i32.const 1
    if 
      i32.const 1
      if
        br 2
        nop
      else
        br 2
        nop
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test br
    i32.const 1
    if $block (result i64)
      i64.const 1
      br $block
      ;; polymorphic stack fills in missing operand
      i64.add
    else
      i64.const 1
      br $block
      i64.const 1
      i64.sub
    end
    drop

    ;; test br to enclosing block
    i32.const 1
    if $outerBlock (result i64)
      i64.const 1
      i32.const 1
      if (result i64)
        i64.const 1
        br $outerBlock
        i64.const 1
        i64.add
      else
        i64.const 1
        br $outerBlock
        ;; polymorphic stack
        i64.sub
      end
      i64.add
    end
    drop

    ;; test br to enclosing func
    i32.const 1
    if $outerBlock (result i64)
      i32.const 0
      if (result i64)
        i64.const 1
        br $resultsMatch
        ;; polymorphic stack
        i64.add
      else
        i64.const 1
        br $resultsMatch
        i64.const 1
        i64.sub
      end
      br 1
      ;; polymorphic stack
      i64.add
    end
    i64.const 1
    i64.add
  )
)
