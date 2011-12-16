structure MipsFrame : FRAME =
struct
  type frame = {name: Temp.label, formals: bool list, locals: int ref}
  type register = string
  datatype access = InFrame of int (* a memory location at offset x from FP *)
                  | InReg of Temp.temp (* value held in register *)
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val wordsize = 4 (*bytes*)

  (* MIPS Registers : http://en.wikipedia.org/wiki/MIPS_architecture *)
  val ZERO = Temp.newtemp() (* Zero *)
  val AT = Temp.newtemp()   (* Assembler Temporary *)
                            
  val v0 = Temp.newtemp()   (* Return Values *)
  val v1 = Temp.newtemp()   
  val RV = v0
                            
  val a0 = Temp.newtemp()   (* Function Arguments *)
  val a1 = Temp.newtemp()   
  val a2 = Temp.newtemp()   
  val a3 = Temp.newtemp()   
                            
  val t0 = Temp.newtemp()   (* Temporaries *)
  val t1 = Temp.newtemp()   
  val t2 = Temp.newtemp()   
  val t3 = Temp.newtemp()   
  val t4 = Temp.newtemp()   
  val t5 = Temp.newtemp()   
  val t6 = Temp.newtemp()   
  val t7 = Temp.newtemp()   
  val t8 = Temp.newtemp()   
  val t9 = Temp.newtemp()   
                            
  val s0 = Temp.newtemp()   (* Saved Temporaries *)
  val s1 = Temp.newtemp()   
  val s2 = Temp.newtemp()   
  val s3 = Temp.newtemp()   
  val s4 = Temp.newtemp()   
  val s5 = Temp.newtemp()   
  val s6 = Temp.newtemp()   
  val s7 = Temp.newtemp()   
                            
  val GP = Temp.newtemp()
  val SP = Temp.newtemp()   (* Stack Pointer *)
  val FP = Temp.newtemp()   (* Frame Pointer *)
  val RA = Temp.newtemp()   (* Return Address *)

  val registers = ["ZERO", "AT", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1",
                   "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "s0", "s1",
                   "s2", "s3", "s4", "s5", "s6", "s7", "GP", "SP", "FP", "RA"]
  val registerTemps = [ZERO, AT, v0, v1, a0, a1, a2, a3, t0, t1, t2, t3, t4, t5,
                       t6, t7, t8, t9, s0, s1, s2, s3, s4, s5, s6, s7, GP, SP,
                       FP, RA]

  (* Register Lists - Page 208 *)
  val argregs = [a0, a1, a2, a3]
  val calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]
  val callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
  val specialregs = [ZERO, AT, v0, v1, GP, SP, FP, RA]

  val colorable = calleesaves @ callersaves

  val tempMap = 
    List.foldl 
      (fn ((key, value), table) => Temp.Table.enter(table, key, value))
        Temp.Table.empty
          (ListPair.zip(registerTemps, registers))

  fun string(l,s) = 
      (Symbol.name(l) ^ ": .asciiz \"" ^ s ^ "\"\n")
      
  val calldefs = callersaves @ [RA, RV]
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
  fun seq [] = Tree.EXP (Tree.CONST 0)
    | seq [exp] = exp
    | seq (exp :: exps) = (Tree.SEQ (exp, (seq exps)))
  
  fun externalCall (str, left, right) = Tree.TEMP (Temp.newtemp()) (*todo*)
  
  fun procEntryExit1 (frame, stm) =
  let
    val saved = [RA] @ calleesaves
    val temps = map (fn temp => Temp.newtemp ()) saved
    val RS = seq (ListPair.mapEq move (temps, saved))
    val RR = seq (ListPair.mapEq move (saved, temps))
    val stm' = seq [RS, stm, RR]

    fun moveargs (arg, access) =
      let
        val access' = exp access
      in
        Tree.MOVE (access' (Tree.TEMP FP), Tree.TEMP arg)
      end
    
    val funFormals = formals frame
    val viewShift = seq (ListPair.map moveargs (argregs, funFormals))
    
  in
    (case funFormals of
      [] => stm'
      | _  => Tree.SEQ (viewShift, stm'))
  end
  
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