signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env :> ENV = 
  type access = unit ref
  type ty = Type.ty
  base_tenv = foldr Symbol.enter Symbol.empty [
    (base_tenv, Symbol.symbol("int"), Ty.INT),
    (base_tenv, Symbol.symbol("string"), Ty.STRING)
  ]
  base_venv = foldr Symbol.enter Symbol.empty [
    (base_venv, Symbol.symbol("print"), FunEntry {formals=[ty.STRING], result=ty.UNIT}),
    (base_venv, Symbol.symbol("flush"), FunEntry {formals=[], result=ty.UNIT}),
    (base_venv, Symbol.symbol("getchar"), FunEntry {formals=[], result=ty.STRING}),
    (base_venv, Symbol.symbol("ord"), FunEntry {formals=[ty.STRING], result=ty.INT}),
    (base_venv, Symbol.symbol("chr"), FunEntry {formals=[ty.INT], result=ty.STRING}),
    (base_venv, Symbol.symbol("size"), FunEntry {formals=[ty.STRING], result=ty.INT}),
    (base_venv, Symbol.symbol("substring"), FunEntry {formals=[ty.STRING,ty.INT,ty.INT], result=ty.STRING}),
    (base_venv, Symbol.symbol("concat"), FunEntry {formals=[ty.STRING,ty.STRING], result=ty.STRING}),
    (base_venv, Symbol.symbol("not"), FunEntry {formals=[ty.INT], result=ty.INT}),
    (base_venv, Symbol.symbol("exit"), FunEntry {formals=[ty.INT], result=ty.UNIT})
  ]

end