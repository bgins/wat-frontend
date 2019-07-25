(module
  (type $binop (func (param i32) (param i32) (result i32)))

  ;; test TypeUse
  (import "env" "add" (func $add (type $binop)))
  (import "env" "sub" (func $sub (type 0)))

  ;; test TypeUseWithDeclarations
  (import "env" "mul" (func $mul (type $binop) (param $lhs i32) (param $rhs i32) (result i32)))
  (import "env" "div" (func $div (type 0) (param $lhs i32) (param $rhs i32) (result i32)))

  ;; test InlineType
  (import "env" "or" (func $or (param i32) (param i32) (result i32)))
  (import "env" "xor" (func $xor (param $lhs i32) (param $rhs i32) (result i32)))
  )  