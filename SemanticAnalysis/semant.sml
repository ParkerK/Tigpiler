signature SEMANT =
sig
  type ty
  type S
  val tenv : ty S.table
  val venv : enventry S.table
  val expty = {exp: Translate.exp, ty: ty}
  val transProg : Absyn.exp -> unit
  val transVar : venv * tenv * Absyn.var -> expty
  val transExp : venv * tenv * Absyn.exp -> expty
  val transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy :         tenv * Absyn.ty  -> ty
end

structure Semant :> SEMANT =
  type ty = Type.ty
  type S = Symbol
  fun transProg(absyn) = 
    let in Type.transExp(venv, tenv) end
  fun transVar(venv, tenv, var) =
    
  fun transExp(venv, tenv, exp) =
    
  fun transDec(venv, tenv, dec) =
    
  fun transTy() =
    
end