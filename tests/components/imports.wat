(module
  (import "console" "log" (func $log (param i32)))
  (import "console" "error" (func $error (param i32)))
  (import "extern" "constVal" (global $constVal i32))
  (import "extern" "mutVal" (global $mutVal (mut i32)))
)