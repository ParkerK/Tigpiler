signature TRANSLATE = 
sig
  (*val simpleVar : access * level -> exp*)
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  val unEx : exp -> Tree.exp
  (*val unNx : exp -> Tree.stm*)
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
end
structure Translate : TRANSLATE = struct 
  structure T = Tree
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  fun seq([]) = T.LABEL(Temp.newlabel())
    | seq(h::t) = T.SEQ(h,seq(t))
    
  fun unEx(Ex e) = e
    | unEx(Cx genstm) =
        let 
          val r = Temp.newtemp()
          val t = Temp.newlabel() and f = Temp.newlabel()
        in
            T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                               genstm(t,f),
                               T.LABEL f,
                               T.MOVE(T.TEMP r, T.CONST 0),
                               T.LABEL t],
                      T.TEMP r)
        end
    | unEx(Nx s) = T.ESEQ(s, T.CONST 0)
  
  exception Impossible of string
  fun unCx(Ex e) = (fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
    | unCx(Cx genstm) = genstm
    | unCx(Nx _) = raise Impossible("Cannot unCx an Nx")
end