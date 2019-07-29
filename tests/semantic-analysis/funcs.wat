(module
  (type $nop (func))
  (type $unop (func (param i32) (result i32)))
  ;; (type $drop (func (param i32)))

  (func $nop (type 0)
    nop
  )

  ;; (func $drop (type 1)
  ;;   i32.const 42
  ;;   drop
  ;;   nop
  ;; )

  (func $fourtyTwo (result i32)
    i32.const 21 
    i32.const 21 
    i32.add
  )

  (func $addOne (type 1)
    local.get 0
    i32.const 21 
    i32.add
  )


)