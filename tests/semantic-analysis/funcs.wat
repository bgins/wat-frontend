(module
  (type $binop (func (param i32) (param i32) (result i32)))

  ;; test TypeUse
  (func $add (type 0)
    local.get 0
    local.get 1
    i32.add
  )
  (func $sub (type $binop)
    local.get 0
    local.get 1
    i32.sub
  )

  ;; test TypeUseWithDeclarations
  (func $mul (type 0) (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.mul
  )
  (func $div (type $binop) (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.div_u
  )

  ;; test InlineType
  (func $or (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.or
  )

)