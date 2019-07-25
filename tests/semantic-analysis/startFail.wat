(module
  ;; func reference does not exist
  (start $none)

  ;; register a legitimate start
  (func $add (result i32)
    i32.const 21
    i32.const 21
    i32.add)
  (start $add)

  ;; multiple starts not permitted
  (func $sub (result i32)
    i32.const 21
    i32.const 21
    i32.sub)
  (start $sub)
)