(module
  (type $binop (func (param i32) (param i32) (result i32)))

  ;; test TypeUse
  (import "env" "add" (func (type $binop)))
  (import "env" "sub" (func (type 0)))

  ;; test TypeUseWithDeclarations
  (import "env" "mul" (func (type $binop) (param $lhs i32) (param $rhs i32) (result i32)))
  (import "env" "div" (func (type 0) (param $lhs i32) (param $rhs i32) (result i32)))

  ;; test InlineType
  (import "env" "or" (func (param $lhs i32) (param $rhs i32) (result i32)))
  )  