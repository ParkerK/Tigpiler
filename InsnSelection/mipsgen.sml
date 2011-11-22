signature CODEGEN =
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure Frame : FRAME = MipsFrame
  structure T = Tree
  
  fun getTempName(t:Temp.temp) = 
    case Symbol.look(Frame.tempMap, t) of
      SOME(str) => str
    | NONE => Temp.makestring(t) (*handle fp?*)

  fun codegen(frame) = fn(stm) => []
  
  fun munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
        (munchExp(e1); munchExp(e2); emit "STORE")
    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2))
        (munchExp(e1); munchExp(e2); emit "STORE")
    | munchStm(T.MOVE(T.MEM(e1), T.MEM(e2))) =
        (munchExp(e1); munchExp(e2); emit "MOVEM")
    | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
        (munchExp(e2); emit "STORE")
    | munchStm(T.MOVE(T.MEM(e1), e2)) =
        (munchExp(e1); munchExp(e2); emit "STORE")
    | munchStm(T.MOVE(T.TEMP i, e2)) =
        (munchExp(e2); emit "ADD")
  and munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
        (munchExp(e1); emit "LOAD")
    | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
        (munchExp(e1); emit "LOAD")
    | munchExp(T.MEM(T.CONST i)) =
        (emit "LOAD")
    | munchExp(T.MEM(e1)) =
        (munchExp(e1); emit "LOAD")
    | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
        (munchExp(e1); emit "ADDI")
    | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
        (munchExp(e1); emit "ADDI")
    | munchExp(T.CONST i) =
        (emit "ADDI")
    | munchExp(T.BINOP(T.PLUS, e1, e2)) =
        (munchExp(e1); munchExp(e2); emit "ADD")
    | munchExp(T.TEMP t) = ()
    
end