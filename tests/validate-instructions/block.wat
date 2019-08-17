(module
  (func
    ;; test block that does nothing
    block $nop
    end

    ;; test nested block that does nothing
    block $nop
      block $innerNop
      end
    end

    ;; test block with result
    block $const (result i32)
      i32.const 1
    end
    drop

    ;; test nested const leaves result
    block $nestedConst (result i32)
      block (result i32)
        i32.const 1
      end
    end
    drop

    ;; test nested block with no outer result
    block $nestedConst
      block (result i32)
        i32.const 1
      end
      drop
    end

    ;; test nested block with no inner result
    block $nestedConst (result i32)
      block
        i32.const 1
        drop
      end
      i32.const 2
    end
    drop
  )
)