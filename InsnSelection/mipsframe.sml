structure MipsFrame : FRAME =
struct
  type frame = {name: Temp.label, formals: bool list, locals: int ref}
  type register = Temp.temp
  datatype access = InFrame of int (*a memory location at offset x from FP*)
                  | InReg of Temp.temp (*value held in register*)
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val tempMap = register Temp.Table
  
  
  val ZERO = Temp.newtemp() (* r0, zero *)
  val FP = Temp.newtemp()   (* Framepointer *)
  val SP = Temp.newtemp()   (* Stackpointer *)
  val RA = Temp.newtemp()   (* Return address *)
  val RV = Temp.newtemp()   (* Return value *)
  
  val wordsize = 4 (*bytes*)
  
  (* Register Lists - Page 208 *)
  val specialregs = [ZERO, FP, SP, RA, RV]
  (* Create Regs *)
  val argregs = List.tabulate (4, (fn _ => Temp.newtemp ()))      (* [a0,a1,a2,a3] *)
  val calleesaves = List.tabulate (8, (fn _ => Temp.newtemp ()))  (* [s0,...,s7] *)
  val callersaves = List.tabulate (10, (fn _ => Temp.newtemp()))  (* [t0,...,t9] *)
  
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
  fun exp(InFrame(k)) = 
    (fn (fp) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k))))
  | exp (InReg(temp)) = (fn (fp) => Tree.TEMP(temp)) 
    
  fun string (label, str) = 
      (Symbol.name label  ^ ": .asciiz \"" ^ str ^ "\"\n")
    
  fun move (reg, var) = Tree.MOVE (Tree.TEMP reg, Tree.TEMP var)
  
  fun externalCall (str, left, right) = Tree.TEMP (Temp.newtemp()) (*todo*)
  fun procEntryExit1 (frame, stm) = stm (*later*)
  
  structure A = Assem
  (* Pg 209 *)
  fun procEntryExit2 (frame,body) =
      body @ 
      [A.OPER{assem="",
              src=specialregs @ calleesaves,
              dst=[],jump=SOME[]}]
  
  (* Pg 209 *)           
  fun procEntryExit3 ({name=name, formals=formals, locals=locals}:frame, 
                      body : Assem.instr list) =
      {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
       body = body,
       epilog = "END " ^ Symbol.name name ^ "\n"}

end