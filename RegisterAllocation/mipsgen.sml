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
  structure F = Frame
  
  fun int (i) =
    if
      i >= 0
    then
      Int.toString i
    else
      "-" ^ Int.toString(~i)
  
  fun codegen(frame) (stm: Tree.stm) : Assem.instr list = 
  let 
    val ilist = ref (nil: A.instr list)
    fun emit x = ilist := x :: (!ilist)
    fun result(gen) = 
      let
        val t = Temp.newtemp()
      in
        gen t; t
      end

   fun operToJump oper = case oper of
      T.EQ => "beq"
    | T.NE => "bne"
    | T.LT => "blt"
    | T.GT => "bgt"
    | T.LE => "ble"
    | T.GE => "bge"
    | _ => "bad branch operator!"
    
  fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) =
        emit(A.OPER{assem="sw `s1, (`s0+" ^ int i ^ ")\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[],jump=NONE})

    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) =
      emit(A.OPER{assem="sw `s1, (`s0+" ^ int i ^ ")\n",
                  src=[munchExp e1, munchExp e2],
                         dst=[],jump=NONE})

    | munchStm(T.MOVE(T.MEM(e1),T.MEM(e2))) =
        emit(A.OPER{assem="move (`s0],  M[`s1)\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[], jump=NONE})
      
    | munchStm(T.MOVE(T.MEM(T.CONST i),e2)) =
        emit(A.OPER{assem="sw `s0, (r0+" ^ int i ^ ")\n",
                    src=[munchExp e2],
                    dst=[], jump=NONE})
      
    | munchStm(T.MOVE(T.MEM(e1),e2)) =
        emit(A.OPER{assem="sw `s1, (`s0)\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[], jump=NONE})

    | munchStm(T.MOVE(T.TEMP i, e2)) =
        emit(A.OPER{assem="add  `d0, `s, r0\n",
          src=[munchExp e2],
          dst=[i], jump=NONE})

    | munchStm(T.LABEL lab) =
      emit(A.LABEL{assem=Symbol.name(lab) ^ ":\n", lab=lab})
      
    
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
        
        
    | munchStm(T.EXP(T.CALL(T.NAME(lab),args))) = 
         emit(A.OPER{assem="jal " ^ Symbol.name(lab) ^ "\n",
         src=munchArgs(0,args), dst=F.RA::F.ZERO::F.callersaves, jump=NONE})
     
    | munchStm (T.EXP exp) = (munchExp exp; ())
    
    
    | munchStm(_) = emit(A.OPER{assem="bad munch stm! line 107", src=[], dst=[], jump=NONE})

    and munchArgs(i,[]) = []
      | munchArgs(i,eh::et) =
        if(i > 0 andalso i < 5)
        then
            let
                val reg = "`a" ^ int (i-1)
                val r = List.nth(F.argregs,(i-1))
            in            
                (emit(A.OPER{assem="addi " ^ reg ^ ", `s0, 0\n",
                 src=[munchExp eh], dst=[r], jump=NONE});
                 r::munchArgs(i+1,et))
            end       
        else
          munchArgs(i+1,et)
    
    and munchExp(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) =
        result(fn r => emit(A.OPER
          {assem="lw `d0, (`s0+" ^ int i ^ ")\n",
           src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) =
              result(fn r => emit(A.OPER
              {assem="lw `d0, (`s0" ^ int i ^ ")\n",
                src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.MEM(T.CONST i)) = 
        result(fn r => emit(A.OPER
          {assem="lw `d0, (r0" ^ int i ^ ")\n",
           src=[], dst=[r], jump=NONE}))

      | munchExp(T.MEM(e1)) = 
        result(fn r => emit(A.OPER
          {assem="lw `d0, (`s0+0)\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      (* PLUS *)

      | munchExp(T.BINOP(T.PLUS,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="addi `d0, `s0, " ^ int i ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.PLUS,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="addi `d0, `s0, " ^ int i ^ "\n",
        src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.PLUS,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="add `d0, `s0, `s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      
      (* MINUS *)
      
      | munchExp(T.BINOP(T.MINUS,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="addi `d0, `s0, " ^ int (~i) ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.MINUS,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="addi `d0, `s0, " ^ int i ^ "\n",
        src=[munchExp ((T.BINOP(T.MINUS,T.CONST 0,e1)))], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.MINUS,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="sub `d0, `s0, `s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           

      (* MULTIPLY *)
      | munchExp(T.BINOP(T.MUL,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="mult `d0, `s0, `s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
       (* DIVIDE *)
       | munchExp(T.BINOP(T.DIV,e1,e2)) =
         result(fn r => emit(A.OPER
           {assem="div `d0, `s0, `s1\n",
            src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
      (* AND *)
      | munchExp(T.BINOP(T.AND,e1,T.CONST i)) =
        result(fn r => emit(A.OPER
          {assem="andi `d0, `s0, " ^ int i ^ "\n",
          src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.AND,T.CONST i,e1)) =
        result(fn r => emit(A.OPER
        {assem="andi `d0, `s0, " ^ int i ^ "\n",
        src=[munchExp e1], dst=[r], jump=NONE}))

      | munchExp(T.BINOP(T.AND,e1,e2)) =
        result(fn r => emit(A.OPER
          {assem="and `d0, `s0, `s1\n",
           src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
     (* OR *)
     | munchExp(T.BINOP(T.OR,e1,T.CONST i)) =
       result(fn r => emit(A.OPER
         {assem="ori `d0, `s0, " ^ int i ^ "\n",
         src=[munchExp e1], dst=[r], jump=NONE}))

     | munchExp(T.BINOP(T.OR,T.CONST i,e1)) =
       result(fn r => emit(A.OPER
       {assem="ori `d0, `s0, " ^ int i ^ "\n",
       src=[munchExp e1], dst=[r], jump=NONE}))

     | munchExp(T.BINOP(T.OR,e1,e2)) =
       result(fn r => emit(A.OPER
         {assem="or `d0, `s0, `s1\n",
          src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
           
     | munchExp(T.CONST i) =
       result(fn r => emit(A.OPER
         {assem="addi `d0, r0, " ^ int i ^ "\n",
          src=[], dst=[r], jump=NONE}))
        
     | munchExp(T.TEMP t) = t
     | munchExp(T.NAME n) =  result(fn r => emit(A.OPER
		{assem="addi `d0, r0, " ^ (Symbol.name n) ^ "\n",
		src=[],dst=[r], jump=NONE}))
	 | munchExp(_) = result(fn _ => emit(A.OPER{assem="bad munch exp! line 299", src=[], dst=[], jump=NONE}))
      
    in
      munchStm stm;
      rev(!ilist)
    end
    
  (*fun getTempName(t:Temp.temp) = 
    case Symbol.look(Frame.tempMap, t) of
      SOME(str) => str
    | NONE => Temp.makestring(t) (*handle fp?*)*)

    
end