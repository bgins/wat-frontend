(module
  (func) ;; disallow, needs some typuse
  (func $funcId) ;; disallow as well
  (func (type 1))
  (func (type 1) (param i32))
  (func (type 1) (result i32))
  (func (type 1) (param i32) (result i32))
  (func $funcId (type 1))
  (func $funcId (type 1) (param i32))
  (func $funcId (type 1) (result i32))
  (func $funcId (type 1) (param i32) (result i32))
  (func $funcId (type $typeId))
  (func $funcId (type $typeId) (param i32))
  (func $funcId (type $typeId) (result i32))
  (func $funcId (type $typeId) (param i32) (result i32))
)