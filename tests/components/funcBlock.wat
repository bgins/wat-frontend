(module
  (func $funcId (type 1) (param i32) (result i32)
  block $blockLabelA (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end $blockLabelA
  block $blockLabelB (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end
  block $blockLabelC
    i32.const 1
    i32.const 2
    i32.add
  end
  block
    i32.const 1
    i32.const 2
    i32.add
  end
  )
) 