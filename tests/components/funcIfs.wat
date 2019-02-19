(module
  (func $funcId (type 1) (param i32) (result i32)
  if $labelA (result i32)
    i32.const 1
  else $labelA
    i32.const 1
  end $labelA
  if $labelB (result i32)
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
  if $labelC (result i32)
    i32.const 1
  end $labelC
  )
)