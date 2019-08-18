;; Should this fail? It does not.

(module
  (func $missingOpd
    loop $block (result i64)
      br $block
      ;; expected operand missing
      nop
    end
    drop
  )
)
