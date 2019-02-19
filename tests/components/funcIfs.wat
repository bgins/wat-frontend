(module
  (func $funcId (type 1) (param i32) (result i32)
  if $label1 (result i32)
    i32.const 1
  else $label1
    i32.const 1
  end $label1
  if $label2 (result i32)
    i32.const 1
  else
    i32.const 1
  end
  if (result i32)
    i32.const 1
  else
    i32.const 1
  end
  if
    i32.const 1
  else
    i32.const 1
  end
  if $label3 (result i32)
    i32.const 1
  end $label3
  )
)