structure MipsFrame : FRAME =
struct
  type frame = {name: Temp.label, formals: bool list, locals: int ref}
  datatype access = InFrame of int (*a memory location at offset x from FP*)
                  | InReg of Temp.temp (*value held in register*)
  
  val ZERO = Temp.newtemp(); (* r0, zero *)
  val FP = Temp.newtemp();   (* Framepointer *)
  val SP = Temp.newtemp();   (* Stackpointer *)
  val RA = Temp.newtemp();   (* Return address *)
  
  val wordsize = 4; (*bytes*)
  
  fun newFrame({name, formals}) = {name=name, formals=formals, locals=ref 0}
  
  fun name(f:frame) = #name f
  
  fun formalToAcc(b:bool, offet:int ref) = case b of 
                                    true => (!offet = !offet + 1; 
                                              InFrame(0 - !offet * wordsize))
                                  | false => InReg(Temp.newtemp())
  fun formals(f:frame) = let
                          val escacc = ref 0
                          fun formalAccs([]) = []
                            | formalAccs(h::r) = formalToAcc(h, escacc)::formalAccs(r)
                        in
                          formalAccs (#formals f)
                        end
  
  fun allocLocal(f:frame) = fn(b) => let
                                        val escacc = #locals f
                                      in
                                        !escacc = !escacc + 1;
                                        formalToAcc(b, escacc)
                                      end
  fun exp(a) = 
    fn(_) => Tree.TEMP(FP) (*todo*)
    
  fun string (label, str) = 
      (Symbol.name label  ^ ": .asciiz \"" ^ str ^ "\"\n")
    
  fun move (reg, var) = Tree.MOVE (Tree.TEMP reg, Tree.TEMP var)
  
end