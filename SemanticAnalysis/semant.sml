structure Semant : sig val transProg : Absyn.exp -> unit end =
struct 
  fun transProg(absyn) = let
    val venv = En.symbol table;
    type tenv = Types.ty Symbol.table
    in 
      Type.transExp(venv, tenv)
    end
  
end



