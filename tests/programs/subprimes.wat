(module
  (import "env" "print" (func $print (param i32)))
  (func $main
    (local $0 i32)
    (local $1 i32)
    (local $2 i32)
    (local $3 i32)
    (local $4 i32)
    (local $5 i32)
    block
      i32.const 100000
      local.set $0
      i32.const 0
      local.set $1
      i32.const 2
      local.set $2
      block
        loop
          i32.const 1
          local.get $2
          local.get $0
          i32.le_s
          i32.sub
          br_if 1
          block
            i32.const 1
            local.set $3
            i32.const 2
            local.set $4
            block
              loop
                i32.const 1
                local.get $4
                local.get $2
                i32.const 1
                i32.sub
                i32.le_s
                i32.sub
                br_if 1
                block
                  local.get $4
                  local.get $2
                  local.get $4
                  i32.div_s
                  i32.mul
                  local.set $5
                  local.get $2
                  local.get $5
                  i32.le_s
                  if
                    i32.const 0
                    local.set $3
                  else
                    nop
                  end
                  local.get $4
                  i32.const 1
                  i32.add
                  local.set $4
                end
                br 0
              end
            end
            local.get $3
            i32.const 0
            i32.ne
            if
              local.get $1
              local.get $2
              i32.add
              local.set $1
            else
              nop
            end
            local.get $2
            i32.const 1
            i32.add
            local.set $2
          end
          br 0
        end
      end
      local.get $1
      call $print
    end)
  (export "main" (func $main)))