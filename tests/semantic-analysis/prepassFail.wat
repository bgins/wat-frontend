(module
  (func $main
    i32.const 21
    i32.const 21
    i32.add
    call $print)
  ;; (global (mut i32) i32.const 1)

  ;; func reference does not exist
  (export "main" (func $none))
)