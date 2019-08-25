(module
  (func $nop
    nop
  )

  (func $drop (param i64)
    local.get 0
    drop
  )

  (func $addOne (param i32) (result i32)
    local.get 0
    i32.const 1
    i32.add
  )

  (func $sub (param i32) (param i32) (result i32)
    local.get 0
    local.get 1
    i32.sub
  )

  (func $one (result i32)
    i32.const 1
  )

  (func $main (result i32)
    call $nop

    i64.const 42
    call $drop

    i32.const 1
    call $addOne
    call 2

    i32.const 1
    call $sub
    i32.const 1
    call 3

    call $one
    drop
  )
)