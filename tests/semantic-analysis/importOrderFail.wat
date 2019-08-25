(module
  (func $main (result i32)
    i32.const 21
    i32.const 21
    i32.add)

  ;; import out of order
  (import "env" "print" (func $print (param i32)))
)