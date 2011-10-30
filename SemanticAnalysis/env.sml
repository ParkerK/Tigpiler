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
  base_tenv = 
  Symbol.enter(base_tenv, int, Ty.INT);
  Symbol.enter(base_tenv, string, Ty.STRING);
  base_venv = 
  Symbol.enter(base_venv, ty.NAME("print",ty.STRING), FunEntry {formals=[ty.STRING], result=ty.UNIT});
  Symbol.enter(base_venv, ty.NAME("flush",ty.STRING), FunEntry {formals=[], result=ty.UNIT});
  Symbol.enter(base_venv, ty.NAME("getchar",ty.STRING), FunEntry {formals=[], result=ty.STRING});
  Symbol.enter(base_venv, ty.NAME("ord",ty.STRING), FunEntry {formals=[ty.STRING], result=ty.INT});
  Symbol.enter(base_venv, ty.NAME("chr",ty.STRING), FunEntry {formals=[ty.INT], result=ty.STRING});
  Symbol.enter(base_venv, ty.NAME("size",ty.STRING), FunEntry {formals=[ty.STRING], result=ty.INT});
  Symbol.enter(base_venv, ty.NAME("substring",ty.STRING), FunEntry {formals=[ty.STRING,ty.INT,ty.INT], result=ty.STRING});
  Symbol.enter(base_venv, ty.NAME("concat",ty.STRING), FunEntry {formals=[ty.STRING,ty.STRING], result=ty.STRING});
  Symbol.enter(base_venv, ty.NAME("not",ty.STRING), FunEntry {formals=[ty.INT], result=ty.INT});
  Symbol.enter(base_venv, ty.NAME("exit",ty.STRING), FunEntry {formals=[ty.INT], result=ty.UNIT});
end