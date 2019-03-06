(module
  (func
    ;; expect 9223372036854775808 as an unsigned 64-bit integer
    i64.const  9223372036854775808
    i64.const -9223372036854775808

    ;; expect 18446744073709551615 as an unsigned 64-bit integer
    i64.const  18446744073709551615
    i64.const -1
    )
)