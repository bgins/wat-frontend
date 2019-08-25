(module
  ;; operand stack cleared by unreachable
  (func
    i32.const 1
    i32.const 2
    i32.const 3
    i32.const 4
    unreachable
  )

  ;; polymorphic stack fabricates an operand
  (func
    unreachable
    i32.const 3
    i32.add
    drop
  )
)