(module
  (type $binop (func (param i32) (param i32) (result i32)))

  ;; test TypeUse, type not defined
  (import "env" "add" (func (type $unop)))
  (import "env" "sub" (func (type 1)))

  ;; test TypeUseWithDeclarations, type not defined
  (import "env" "mul" (func (type $unop) (param $lhs i32) (param $rhs i32) (result i32)))
  (import "env" "div" (func (type 1) (param $lhs i32) (param $rhs i32) (result i32)))

  ;; test TypeUseWithDeclarations, mismatched params and results
  (import "env" "and" (func (type $binop) (param $lhs i64) (param $rhs i32) (result i32)))
  (import "env" "or" (func (type 0) (param $lhs i32) (param $rhs i32) (result i64)))


  ;; test InlineType
  (import "env" "xor" (func (param $lhs i64) (param $rhs i32) (result i32)))

  ;; redefinitions

  )