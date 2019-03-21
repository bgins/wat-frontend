module Keywords ( keywords ) where

import Data.List

{-| Collect keywords into a list of strings where the longest keywords appear
  first.

  This arrangement is to allow match on longest keywords first, which is not
  particularly efficient. Sorting can be dropped when the lexer is changes its
  matching strategy.

  This is an implementation of the WebAssembly spec released on January 10th,
  2019. Sections of the specification are referenced with § section marks
  throughout this module.
-}

keywords :: [String]
keywords =
    reverse $ sort $ concat
        [ types 
        , instructions
        , modules
        ]



--TYPES


types :: [String]
types =
    concat
        [ values
        , functions
        , tables
        , globals
        ]


-- [§6.4.1]
values :: [String]
values =
    [ "i32"
    , "i64"
    , "f32"
    , "f64"
    ]


-- [§6.4.3]
functions :: [String]
functions =
    [ "func"
    , "param"
    , "result"
    ]


-- [§6.4.6]
tables :: [String]
tables =
    [ "funcref" ]


-- [§6.4.7]
globals :: [String]
globals =
    [ "mut" ]



-- INSTRUCTIONS


instructions :: [String]
instructions =
    concat
        [ control
        , parametric
        , variable
        , memory
        , numeric
        , folded
        ]


-- [§6.5.2]
control :: [String]
control =
    [ "block"
    , "loop"
    , "if"
    , "else"
    , "end"
    , "unreachable"
    , "nop"
    , "br"
    , "br_if"
    , "br_table"
    , "return"
    , "call"
    , "call_indirect"
    ]


-- [§6.5.3]
parametric :: [String]
parametric =
    [ "drop"
    , "select"
    ]


-- [§6.5.4]
variable :: [String]
variable =
    [ "local.get"
    , "local.set"
    , "local.tee"
    , "global.get"
    , "global.set"
    ]


-- [§6.5.5]
memory :: [String]
memory =
    [
    -- "offset="
    -- "align="
    "i32.load"
    , "i64.load"
    , "f32.load"
    , "f64.load"
    , "i32.load8_s"
    , "i32.load8_u"
    , "i32.load16_s"
    , "i32.load16_u"
    , "i64.load8_s"
    , "i64.load8_u"
    , "i64.load16_s"
    , "i64.load16_u"
    , "i64.load32_s"
    , "i64.load32_u"
    , "i32.store"
    , "i64.store"
    , "f32.store"
    , "f64.store"
    , "i32.store8"
    , "i32.store16"
    , "i64.store8"
    , "i64.store16"
    , "i64.store32"
    , "memory.size"
    , "memory.grow"
    ]


-- [§6.5.6]
numeric :: [String]
numeric =
    [ "i32.const"
    , "i64.const"
    , "f32.const"
    , "f64.const"
    , "i32.clz"
    , "i32.ctz"
    , "i32.popcnt"
    , "i32.add"
    , "i32.sub"
    , "i32.mul"
    , "i32.div_s"
    , "i32.div_u"
    , "i32.rem_s"
    , "i32.rem_u"
    , "i32.and"
    , "i32.or"
    , "i32.xor"
    , "i32.shl"
    , "i32.shr_s"
    , "i32.shr_u"
    , "i32.rotl"
    , "i32.rotr"
    , "i64.clz"
    , "i64.ctz"
    , "i64.popcnt"
    , "i64.add"
    , "i64.sub"
    , "i64.mul"
    , "i64.div_s"
    , "i64.div_u"
    , "i64.rem_s"
    , "i64.rem_u"
    , "i64.and"
    , "i64.or"
    , "i64.xor"
    , "i64.shl"
    , "i64.shr_s"
    , "i64.shr_u"
    , "i64.rotl"
    , "i64.rotr"
    , "f32.abs"
    , "f32.neg"
    , "f32.ceil"
    , "f32.floor"
    , "f32.trunc"
    , "f32.nearest"
    , "f32.sqrt"
    , "f32.add"
    , "f32.sub"
    , "f32.mul"
    , "f32.div"
    , "f32.min"
    , "f32.max"
    , "f32.copysign"
    , "f64.abs"
    , "f64.neg"
    , "f64.ceil"
    , "f64.floor"
    , "f64.trunc"
    , "f64.nearest"
    , "f64.sqrt"
    , "f64.add"
    , "f64.sub"
    , "f64.mul"
    , "f64.div"
    , "f64.min"
    , "f64.max"
    , "f64.copysign"
    , "i32.eqz"
    , "i32.eq"
    , "i32.ne"
    , "i32.lt_s"
    , "i32.lt_u"
    , "i32.gt_s"
    , "i32.gt_u"
    , "i32.le_s"
    , "i32.le_u"
    , "i32.ge_s"
    , "i32.ge_u"
    , "i64.eqz"
    , "i64.eq"
    , "i64.ne"
    , "i64.lt_s"
    , "i64.lt_u"
    , "i64.gt_s"
    , "i64.gt_u"
    , "i64.le_s"
    , "i64.le_u"
    , "i64.ge_s"
    , "i64.ge_u"
    , "f32.eq"
    , "f32.ne"
    , "f32.lt"
    , "f32.gt"
    , "f32.le"
    , "f32.ge"
    , "f64.eq"
    , "f64.ne"
    , "f64.lt"
    , "f64.gt"
    , "f64.le"
    , "f64.ge"
    , "i32.wrap_i64"
    , "i32.trunc_f32_s"
    , "i32.trunc_f32_u"
    , "i32.trunc_f64_s"
    , "i32.trunc_f64_u"
    , "i64.extend_i32_s"
    , "i64.extend_i32_u"
    , "i64.trunc_f32_s"
    , "i64.trunc_f32_u"
    , "i64.trunc_f64_s"
    , "i64.trunc_f64_u"
    , "f32.convert_i32_s"
    , "f32.convert_i32_u"
    , "f32.convert_i64_s"
    , "f32.convert_i64_u"
    , "f32.demote_f64"
    , "f64.convert_i32_s"
    , "f64.convert_i32_u"
    , "f64.convert_i64_s"
    , "f64.convert_i64_u"
    , "f64.promote_f32"
    , "i32.reinterpret_f32"
    , "i64.reinterpret_f64"
    , "f32.reinterpret_i32"
    , "f64.reinterpret_i64"
    ]


-- [§6.5.7]
folded :: [String]
folded =
    [ "then" ]



-- MODULES


-- [§6.6]
modules :: [String]
modules =
    [ "type"
    , "import"
    , "local"
    , "table"
    , "memory"
    , "global"
    , "export"
    , "start"
    , "elem"
    , "offset"
    , "data"
    , "module"
    ]
