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
  
  
  fun codegen(frame) (stm: Tree.stm) : Assem.instr list = 
  let 
    val ilist = ref (nil: A.instr list)
    fun emit x = ilist := x :: (!ilist)
    result(gen) = 
      let
        val t = Temp.newtemp()
      in
        gen t; t
      end

   fun operToJump oper = case oper of
      T.EQ => "BEQ"
    | T.NE => "BNE"
    | T.LT => "BLT"
    | T.GT => "BGT"
    | T.LE => "BLE"
    | T.GE => "BGE"
    
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
      emit(A.LABEL{assem=lab ^ ":\n", lab=lab})
      
    | munchStm (T.EXP(T.CALL(e,args))) =
      emit (A.OPER{assem="CALL `s0\n",
                  src=munchExp(e)::munchArgs(0,args),
                  dst=calldefs,
                  jump=NONE})
    
    (* JUMP *)
     | munchStm(T.CJUMP(oper, T.CONST i, e1, lab1, lab2)) =
      emit(A.OPER{assem=((operToJump (oper)) ^ " `s0," ^ int i ^ "," ^ Symbol.name(lab1) ^ "\n" ^ "j " ^ (Symbol.name lab2) ^ "\n"),
        src=[munchExp e1], dst=[], jump=SOME([lab1,lab2])})

    | munchStm(T.CJUMP(oper, e1, T.CONST i, lab1, lab2)) =
            emit(A.OPER{assem=((operToJump oper) ^ " `s0," ^ int i ^ "," ^ Symbol.name(lab1) ^ "\n" ^ "j " ^ (Symbol.name lab2) ^ "\n"),
                        src=[munchExp e1], dst=[], jump=SOME([lab1,lab2])})

    | munchStm(T.CJUMP(oper, e1, e2, lab1, lab2)) =
      emit(A.OPER{assem=((operToJump oper) ^ " `s0,`s1," ^ Symbol.name(lab1) ^ "\n" ^ "j " ^ (Symbol.name lab2) ^ "\n"),
        src=[munchExp e1, munchExp e2], dst=[], jump=SOME([lab1,lab2])})

    | munchStm(T.JUMP(T.NAME(lab), llst)) =
      emit(A.OPER{assem = "j " ^ (Symbol.name lab) ^ "\n",
        src=[], dst=[], jump=SOME[Temp.namedlabel(Symbol.name lab)]})
  
    and munchExp(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) =
        result(fn r => emit(A.OPER
          {assem="LOAD `d0 <- M[`s0+" ^ int i ^ "]\n",
           src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) =
              result(fn r => emit(A.OPER
              {assem="LOAD `d0 <- M[`s0" ^ int i"]\n",
                src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.MEM(T.CONST i)) = 
        result(fn r => emit(A.OPER
          {assem="LOAD `d0 <- M[r0" ^ int i ^ "]\n",
           src=[], dst=[r], jump=NONE}))

      | munchExp(T.MEM(e1)) = 
        result(fn r => emit(A.OPER
          {assem="LOAD `d0 <- M[`s0+0]\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      (* PLUS *)

      | munchExp(T.BINOP(T.PLUS,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="ADDI `d0 <- `s0 +" ^ int i ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.PLUS,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="ADDI `d0 <- `s0 +" ^ int i ^ "\n",
        src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.PLUS,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="ADD `d0 <- `s0+`s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      
      (* MINUS *)
      
      | munchExp(T.BINOP(T.MINUS,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="ADDI `d0 <- `s0 +" ^ int (~i) ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.MINUS,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="ADDI `d0 <- `s0 +" ^ int i ^ "\n",
        src=[munchExp ((T.BINOP(T.MINUS,T.CONST 0,e1)))], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.MINUS,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="SUB `d0 <- `s0+`s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           

      (* MULTIPLY *)
      | munchExp(T.BINOP(T.MUL,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="MULT `d0 <- `s0+`s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
       (* DIVIDE *)
       | munchExp(T.BINOP(T.DIV,e1,e2)) =
         result(fn r => emit(A.OPER
           {assem="DIV `d0 <- `s0+`s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
      (* AND *)
      | munchExp(T.BINOP(T.AND,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="ANDI `d0 <- `s0 +" ^ int i ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.AND,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="ANDI `d0 <- `s0 +" ^ int i ^ "\n",
        src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.AND,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="AND `d0 <- `s0+`s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
     (* OR *)
     | munchExp(T.BINOP(T.OR,e1,T.CONST i)) =
       result(fn r => emit(A.OPER
         {assem="ORI `d0 <- `s0 +" ^ int i ^ "\n",
         src=[munchExp e1], dst=[r], jump=NONE}))

     | munchExp(T.BINOP(T.OR,T.CONST i,e1)) =
       result(fn r => emit(A.OPER
       {assem="ORI `d0 <- `s0 +" ^ int i ^ "\n",
       src=[munchExp e1], dst=[r], jump=NONE}))

     | munchExp(T.BINOP(T.OR,e1,e2)) =
       result(fn r => emit(A.OPER
         {assem="OR `d0 <- `s0+`s1\n",
          src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
     | munchExp(T.CONST i) =
       result(fn r => emit(A.OPER
         {assem="ADDI `d0 <- r0+" ^ int i ^ "\n",
          src=[], dst=[r], jump=NONE}))
        
      | munchExp(T.TEMP t) = t
    
    in
      munchStm stm;
      rev(!ilist)
    end
    
  fun getTempName(t:Temp.temp) = 
    case Symbol.look(Frame.tempMap, t) of
      SOME(str) => str
    | NONE => Temp.makestring(t) (*handle fp?*)

    
end