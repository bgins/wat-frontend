(module
  (func $labelTests
    ;; test br_if
    i32.const 1
    if $block
      i32.const 1
      br_if $block
      nop
    else
      i32.const 0
      br_if $block
      nop
    end

    ;; test br_if to enclosing loop
    i32.const 1
    if $outerBlock
      i32.const 1
      if
        i32.const 0
        br_if $outerBlock
        nop
      else
        i32.const 1
        br_if $outerBlock
        nop
      end
      nop
    end

    ;; test br_if to enclosing func
    i32.const 1
    if
      i32.const 1
      if
        i32.const 1
        br_if $labelTests
        nop
      else
        i32.const 0
        br_if $labelTests
        nop
      end
      nop
    end
  )

  (func $indexTests
    ;; test br_if
    i32.const 1
    if
      i32.const 1
      br_if 0
      nop
    else
      i32.const 1
      br_if 0
      nop
    end

    ;; test br_if to enclosing block
    i32.const 1
    if
      i32.const 1
      if
        i32.const 1
        br_if 1
        nop
      else
        i32.const 1
        br_if 1
        nop
      end
      nop
    end

    ;; test br_if to enclosing func
    i32.const 1
    if 
      i32.const 1
      if
        i32.const 1
        br_if 2
        nop
      else
        i32.const 1
        br_if 2
        nop
      end
      nop
    end
  )

  (func $resultsMatch (result i64)
    ;; test br_if
    i32.const 1
    if $block (result i64)
      i64.const 1
      i32.const 0
      br_if $block
      i64.const 1
      i64.add
    else
      i64.const 1
      i32.const 0
      br_if $block
      i64.const 1
      i64.sub
    end
    drop

    ;; test br_if to enclosing block
    i32.const 1
    if $outerBlock (result i64)
      i64.const 1
      i32.const 1
      if (result i64)
        i64.const 1
        i32.const 1
        br_if $outerBlock
        i64.const 1
        i64.add
      else
        i64.const 1
        i32.const 1
        br_if $outerBlock
        i64.const 1
        i64.sub
      end
      i64.add
    end
    drop

    ;; test br_if to enclosing func
    i32.const 1
    if $outerBlock (result i64)
      i32.const 0
      if (result i64)
        i64.const 1
        i32.const 1
        br_if $resultsMatch
        i64.const 1
        i64.add
      else
        i64.const 1
        i32.const 0
        br_if $resultsMatch
        i64.const 1
        i64.sub
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
