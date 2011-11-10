signature TRANSLATE = 
sig
  (*val simpleVar : access * level -> exp*)
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  val unEx : exp -> Tree.exp
  (*val unNx : exp -> Tree.stm*)
  (*val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)*)
end
structure Translate : TRANSLATE = struct 
  
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  fun seq([]) = Tree.LABEL(Temp.newlabel())
    | seq(h::t) = Tree.SEQ(h,seq(t))
    
  fun unEx(Ex e) = e
    | unEx(Cx genstm) =
        let 
          val r = Temp.newtemp()
          val t = Temp.newlabel() and f = Temp.newlabel()
        in
            Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                               genstm(t,f),
                               Tree.LABEL f,
                               Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                               Tree.LABEL t],
                      Tree.TEMP r)
        end
    | unEx(Nx s) = Tree.ESEQ(s, Tree.CONST 0)
    
end