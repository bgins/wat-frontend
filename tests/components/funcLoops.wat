(module
  (func $funcId (type 1) (param i32) (result i32)
  loop $loopLabelA (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end $loopLabelA
  loop $loopLabelB (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end
  loop $loopLabelC
    i32.const 1
    i32.const 2
    i32.add
  end
  loop
    i32.const 1
    i32.const 2
    i32.add
  end
  )
)