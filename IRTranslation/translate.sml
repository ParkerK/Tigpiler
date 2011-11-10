structure Translate = struct 

  val simpleVar : access * level -> exp
  
  type exp = Ex or Tree.exp
            | Nx of Tree.stm
            | Cx of Temp.label * Temp.label -> Tree.stm

end