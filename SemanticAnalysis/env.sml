signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env :> ENV = struct
  type access = unit ref
  type ty = Types.ty
  val base_tenv = foldr Symbol.enter Symbol.empty [
    (base_tenv, Symbol.symbol("int"), Types.INT),
    (base_tenv, Symbol.symbol("string"), Types.STRING)
  ]
  val base_venv = foldr Symbol.enter Symbol.empty [
    (base_venv, Symbol.symbol("print"), FunEntry {formals=[Types.STRING], result=Types.UNIT}),
    (base_venv, Symbol.symbol("flush"), FunEntry {formals=[], result=Types.UNIT}),
    (base_venv, Symbol.symbol("getchar"), FunEntry {formals=[], result=Types.STRING}),
    (base_venv, Symbol.symbol("ord"), FunEntry {formals=[Types.STRING], result=Types.INT}),
    (base_venv, Symbol.symbol("chr"), FunEntry {formals=[Types.INT], result=Types.STRING}),
    (base_venv, Symbol.symbol("size"), FunEntry {formals=[Types.STRING], result=Types.INT}),
    (base_venv, Symbol.symbol("substring"), FunEntry {formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING}),
    (base_venv, Symbol.symbol("concat"), FunEntry {formals=[Types.STRING,Types.STRING], result=Types.STRING}),
    (base_venv, Symbol.symbol("not"), FunEntry {formals=[Types.INT], result=Types.INT}),
    (base_venv, Symbol.symbol("exit"), FunEntry {formals=[Types.INT], result=Types.UNIT})
  ]

end