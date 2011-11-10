structure Translate = struct 
  type exp = Ex or Tree.exp
            | Nx of Tree.stm
            | Cx of Temp.label * Temp.label -> Tree.stm

end