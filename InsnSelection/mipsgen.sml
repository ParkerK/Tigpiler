signature CODEGEN =
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure Frame : FRAME = MipsFrame
  structure T = Tree
  structure A = Assem
  
  fun munchStm(T.SEQ(a,b)) = (mumchStm a; munchStm b)
    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) =
        emit(A.OPER{assem="STORE M[`s0+" ^ int i ^ "] <- `s1\n",
                    src=[munchExp e2, munchExp e3],
                    dst=[],jump=NONE})

    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) =
      emit(A.OPER{assem="STORE M[`s0+" ^ int i ^ "] <- `s1\n",
                  src=[munchExp e1, munchExp e2],
                         dst=[],jump=NONE})

    | munchStm(T.MOVE(T.MEM(e1),T.MEM(e2))) =
        emit(A.OPER{assem="MOVE M[`s0] <- M[`s1]\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[], jump=NONE})
      
    | munchStm(T.MOVE(T.MEM(T.CONST i),e2)) =
        emit(A.OPER{assem="STORE M[r0+" ^ int i ^ "] <- `s0\n",
                    src=[munchExp e2],
                    dst=[], jump=NONE})
      
    | munchStm(T.MOVE(T.MEM(e1),e2)) =
        emit(A.OPER{assem="STORE M[`s0] <- `s1\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[], jump=NONE})

    | munchStm(T.MOVE(T.TEMP i, e2)) =
        emit(A.OPER{assem="ADD  `d0 <- `s + r0\n",
          src=[munchExp e2],
          dst=[i], jump=NONE})

    | munchStm(T.LABEL lab) =
      emit(A.LABEL{assem=lab ^ ":\n", lab=;lab})

  
  fun getTempName(t:Temp.temp) = 
    case Symbol.look(Frame.tempMap, t) of
      SOME(str) => str
    | NONE => Temp.makestring(t) (*handle fp?*)

  fun codegen(frame) = fn(stm) => []
end