(* See Page 207 *)
signature CODEGEN = 
sig
    structure Frame : FRAME = MipsFrame
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end