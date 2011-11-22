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
end