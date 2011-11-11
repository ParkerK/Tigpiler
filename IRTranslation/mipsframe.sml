structure MipsFrame : FRAME =
struct
  structure frame = {formalloc: Temp.label, shift: Temp.temp -> Temp.temp, 
                     locals: int ref, label: Temp.label}
  datatype access = InFrame of int (*a memory location at offset x from FP*)
                  | InReg of Temp.temp (*value held in register*)
  val FP = Temp.newlabel();
  val wordsize = 4;
  
  fun newFrame({lbl, fs}) = (!FP := !FP - length(fs))
  fun name(f) = case frame.look(f) of 
                  SOME() =>
                | NONE =>
  fun formals(f) = access list
  fun allocLocal(f) = fn(b) => case b of 
                                  true => (!FP := !FP + 1; InFrame(!FP))
                                | false => InReg(Temp.newtemp())
  fun exp(a) = 
    fn(Tree.TEMP(FP)) => Tree.Tree(FP) (*todo*)
end