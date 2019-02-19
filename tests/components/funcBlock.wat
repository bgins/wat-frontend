(module
  (func $funcId (type 1) (param i32) (result i32)
  block $blockLabel1 (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end $blockLabel1
  block $blockLabel2 (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end
  block $blockLabel3
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