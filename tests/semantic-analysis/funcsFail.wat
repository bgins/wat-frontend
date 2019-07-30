(module
  (type $binop (func (param i32) (param i32) (result i32)))

  ;; test TypeUse, type not defined
  (func $add (type 1)
    local.get 0
    local.get 1
    i32.add
  )
  (func $clz (type $unop)
    local.get 0
    i32.clz
  )

  ;; test TypeUseWithDeclarations, type not defined
  (func $mul (type 1) (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.mul
  )
  (func $ctz (type $unop) (param $lhs i32) (param $rhs i32) (result i32)
    local.get 0
    i32.clz
  )

  ;; test TypeUseWithDeclarations, mismatched params and results
  (func $sub (type 0) (param $lhs i64) (param $rhs i32) (result i32)
    local.get 0
    local.get 1
    i32.mul
  )
  (func $div (type $binop) (param $lhs i32) (param $rhs i32) (result i64)
    local.get 0
    local.get 1
    i32.div_u
  )

  ;; test redefinition
  (func $sub (type 0)
    local.get 0
    local.get 1
    i32.sub
  )
)