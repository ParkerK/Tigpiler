structure Translate = struct 

  val simpleVar : access * level -> exp
  
  type exp  = Ex of Tree.exp
            | Nx of Tree.stm
            | Cx of Temp.label * Temp.label -> Tree.stm
            
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
    | unEX(Nx s) = Tree.ESEQ(s, Tree.CONST 0)
    
end