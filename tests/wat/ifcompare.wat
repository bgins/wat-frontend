(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    block
      i32.const 1
      i32.const 1
      i32.eq
      i32.const 1
      i32.const 2
      i32.ne
      i32.and
      if
        i32.const 1
        call $print
      else
        i32.const 0
        call $print
      end
      i32.const 1
      i32.const 2
      i32.lt_s
      i32.const 2
      i32.const 1
      i32.gt_s
      i32.and
      if
        i32.const 2
        call $print
      else
        i32.const 0
        call $print
      end
      i32.const 1
      i32.const 1
      i32.le_s
      i32.const 1
      i32.const 1
      i32.ge_s
      i32.and
      if
        i32.const 3
        call $print
      else
        i32.const 0
        call $print
      end
      i32.const 1
      i32.const 2
      i32.le_s
      i32.const 2
      i32.const 1
      i32.ge_s
      i32.and
      if
        i32.const 4
        call $print
      else
        i32.const 0
        call $print
      end
      i32.const 1
      i32.const 2
      i32.eq
      i32.const 1
      i32.const 1
      i32.ne
      i32.or
      if
        i32.const 0
        call $print
      else
        i32.const 1
        call $print
      end
      i32.const 2
      i32.const 1
      i32.lt_s
      i32.const 1
      i32.const 2
      i32.gt_s
      i32.or
      if
        i32.const 0
        call $print
      else
        i32.const 2
        call $print
      end
      i32.const 1
      i32.const 0
      i32.le_s
      i32.const 0
      i32.const 1
      i32.ge_s
      i32.or
      if
        i32.const 0
        call $print
      else
        i32.const 3
        call $print
      end
    end)
  (export "main" (func $main)))