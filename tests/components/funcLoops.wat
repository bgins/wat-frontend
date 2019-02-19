(module
  (func $funcId (type 1) (param i32) (result i32)
  loop $loopLabel1 (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end $loopLabel1
  loop $loopLabel2 (result i32)
    i32.const 1
    i32.const 2
    i32.add
  end
  loop $loopLabel3
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