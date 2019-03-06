(module
  (func
    ;; expect 2147483648 as an unsigned 32-bit integer
    i32.const   2147483648
    i32.const  -2147483648

    ;; expect 4294967295 as an unsigned 32-bit integer
    i32.const   4294967295
    i32.const  -1
    )
)