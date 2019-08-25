;; all tests fallthrough
;; tests that actually loop are in loopBr.wat and loopBrIf.wat

(module
  (func
    ;; test loop that does nothing
    loop $nop
    end

    ;; test nested loop that does nothing
    loop $nop
      loop $innerNop
      end
    end

    ;; test loop with result
    loop $const (result i32)
      i32.const 1
    end
    drop

    ;; test nested loop leaves result
    loop $nestedConst (result i32)
      loop (result i32)
        i32.const 1
      end
    end
    drop

    ;; test nested loop with no outer result
    loop $nestedConst
      loop (result i32)
        i32.const 1
      end
      drop
    end

    ;; test nested loop with no inner result
    loop $nestedConst (result i32)
      loop
        i32.const 1
        drop
      end
      i32.const 2
    end
    drop
  )
)