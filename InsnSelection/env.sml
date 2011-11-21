signature ENV =
sig
  type access
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty, label: Temp.label, level: Translate.level}
  val base_tenv : Types.ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env :> ENV = struct
  type access = unit ref
  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {formals: Types.ty list, result: Types.ty, label: Temp.label, level: Translate.level}
                    
  fun enter ((symbol, entry), env) = Symbol.enter(env, symbol, entry)
  
  val init_level = Translate.newLevel {parent=Translate.outermost, name=Temp.namedlabel("init"), formals=[]}
  
  val base_tenv = foldr enter Symbol.empty [
    (Symbol.symbol("int"), Types.INT),
    (Symbol.symbol("string"), Types.STRING)
  ]

  val base_venv = foldr enter Symbol.empty [
    (Symbol.symbol("print"),      FunEntry {formals=[Types.STRING], result=Types.UNIT, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("flush"),      FunEntry {formals=[], result=Types.UNIT, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("getchar"),    FunEntry {formals=[], result=Types.STRING, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("ord"),        FunEntry {formals=[Types.STRING], result=Types.INT, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("chr"),        FunEntry {formals=[Types.INT], result=Types.STRING, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("size"),       FunEntry {formals=[Types.STRING], result=Types.INT, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("substring"),  FunEntry {formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("concat"),     FunEntry {formals=[Types.STRING,Types.STRING], result=Types.STRING, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("not"),        FunEntry {formals=[Types.INT], result=Types.INT, label= Temp.newlabel(), level=init_level}),
    (Symbol.symbol("exit"),       FunEntry {formals=[Types.INT], result=Types.UNIT, label= Temp.newlabel(), level=init_level})
  ]

end