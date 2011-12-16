signature TRANSLATE = 
sig
  type access (*not the same as Frame.access*)
  type level 
  type frag
  type breakpoint
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm

  val frags : frag list ref
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
  val seq : Tree.stm list ->Tree.stm
  val newbreakpoint : unit -> breakpoint
  val assignExp : exp * exp -> exp
  val breakExp : Tree.label -> exp
  val intExp : int -> exp
  val nilExp : unit -> exp
  val ifThenExp : exp * exp -> exp
  val ifThenElseExp : exp * exp * exp -> exp
  val intOpExp : Absyn.oper * exp * exp -> exp
  val letExp : exp list * exp -> exp
  val seqExp : exp list -> exp
  val stringExp : string -> exp
  val stringOpExp : Absyn.oper * exp * exp -> exp
  val whileExp : exp * exp * Tree.label -> exp
  val forExp : exp * Tree.label * exp * exp * exp -> exp
  val callExp : level * Tree.label * exp list  -> exp
  val recordExp : exp list  -> exp
  
  val simpleVar : access * level -> exp
  val subscriptExp : exp * exp -> exp
  val fieldVar : exp * exp -> exp
  val empty : exp

  val procEntryExit: {level: level, body: exp} -> unit
  val getResult : unit -> frag list
  (*val breakExp : exp -> Temp.label*)
  
end

structure Translate : TRANSLATE = struct 
  structure Frame : FRAME = MipsFrame
  structure A = Absyn
  structure T = Tree
  type breakpoint = Tree.label
  val err = ErrorMsg.error
  exception ErrMsg
  
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  datatype level =  Top
                  | Level of {frame:Frame.frame, parent: level} * unit ref
  type access = level * Frame.access
  val outermost = Top
  type frag = Frame.frag
  val frags = ref([] : frag list)
  val newbreakpoint = Temp.newlabel
  
  exception Impossible of string
  fun newLevel({parent=l, name=n, formals=f}) = (Level ({frame=Frame.newFrame {name=n, formals=true::f}, parent=l}, ref())) (*change level?*)
  fun formals(Top) = []
    | formals(l as Level({frame, parent}, uref)) =
        let val fs = tl(Frame.formals(frame)) (*remove the true value for static link*)
             fun convertAccess([]) = []
               | convertAccess(h::accs) = (l, h) :: convertAccess(accs)
         in
             convertAccess(fs)
         end

  fun allocLocal(Top) = raise Impossible ("can't allocate a local variable at the top most scope")
    | allocLocal(l as Level({frame, parent}, uref)) = (fn(b) => (l,Frame.allocLocal(frame)(b)))
  
  fun seq([]) = T.LABEL(Temp.newlabel())
    | seq([s]) = s
    | seq(h::t) = T.SEQ(h,seq(t))
    
  fun unEx(Ex e) = e
    | unEx(Cx genstm) =
        let 
          val r = Temp.newtemp()
          val t = Temp.newlabel() and f = Temp.newlabel()
        in
            T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                               genstm(t,f),
                               T.LABEL f,
                               T.MOVE(T.TEMP r, T.CONST 0),
                               T.LABEL t],
                      T.TEMP r)
        end
    | unEx(Nx s) = T.ESEQ(s, T.CONST 0)
  
  fun unNx(Ex e) = T.EXP(e)
    | unNx(Cx genstm) = 
        let
          val t = Temp.newlabel()
          val f = t
        in
          T.SEQ(genstm(t,f), T.LABEL t)
        end
    | unNx(Nx s) = s
    
  fun unCx(Ex(T.CONST 0)) = (fn (t, f) => T.JUMP(T.NAME f, [f]))
    | unCx(Ex(T.CONST _)) = (fn (t, f) => T.JUMP(T.NAME t, [t]))
    | unCx(Ex e) = (fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
    | unCx(Cx genstm) = genstm
    | unCx(Nx _) = raise Impossible("Cannot unCx an Nx")
  
  fun initExp (acc, lev, init) = Nx (T.MOVE (unEx (acc lev), unEx init))
    
  fun whileExp (test, body, done) = 
    let
      val test = unCx test
      val body = unNx body
      val testLabel = Temp.newlabel()
      val bodyLabel = Temp.newlabel() 
    in 
      Nx (seq [T.LABEL testLabel, 
          test (bodyLabel, done),
          T.LABEL bodyLabel,
          body,
          T.JUMP (T.NAME testLabel, [testLabel]),
          T.LABEL done])
    end
    
    val empty = Ex(T.CONST(0))
    
    
    fun forExp (var, escape, lo, hi, body) = 
      let 
        val var = unEx var
        val lo = unEx lo
        val hi = unEx hi
        val body = unNx body
        val bodyLabel = Temp.newlabel()
        val forLabel = Temp.newlabel()
      in
        Nx (seq [T.MOVE (var, lo), (* Set var := lo *)
          T.CJUMP (T.LE, var, hi, bodyLabel, escape),
          T.LABEL bodyLabel,
          body,
          T.CJUMP (T.LT, var, hi, forLabel, escape), (* Check if exiting loop *)
          T.LABEL forLabel,
          T.MOVE (var, T.BINOP (T.PLUS, var, T.CONST 1)), (* var += 1 *)
          T.JUMP (T.NAME forLabel, [forLabel]),
          T.LABEL escape])
        end
    fun ifThenExp (cond, thenExp) =
        let
          val cond = unCx(cond)
          val thenLabel = Temp.newlabel()
          val endLabel = Temp.newlabel()
          val r = Temp.newtemp()
        in
          case (cond, thenExp) of
                  (_, Cx _) =>
                    Cx (fn (t, f) =>
                           seq [(cond) (thenLabel, endLabel),
                                T.LABEL thenLabel,
                                (unCx thenExp) (t, f),
                                T.LABEL endLabel])
                  | (_, Nx _) =>
                    Nx (seq [(cond) (thenLabel, endLabel),
                             T.LABEL thenLabel,
                             unNx thenExp,
                             T.LABEL endLabel])
                  | (_, Ex ex) => 
                    Ex (T.ESEQ (seq [(cond) (thenLabel, endLabel),
                                     T.LABEL thenLabel,
                                     T.MOVE (T.TEMP r, ex),
                                     T.LABEL endLabel],
                                T.TEMP r))
        end
    fun ifThenElseExp (cond, thenExp, elseExp) =
        let
          val cond = unCx(cond)
          val thenLabel = Temp.newlabel()
          val elseLabel = Temp.newlabel()
          val joinLabel = Temp.newlabel()
          val r = Temp.newtemp() (* Only for when thenExp/elseExp = Ex, suggested on page 162 *)
        in
          case (cond, thenExp, elseExp) of
                  (_, Cx _, Cx _) =>
                    Cx (fn (t, f) =>
                           seq [(cond) (thenLabel, elseLabel),
                                T.LABEL thenLabel,
                                (unCx thenExp) (t, f),
                                T.LABEL elseLabel,
                                (unCx elseExp) (t, f)])
                  | (_, Ex _, Ex _) =>
                    Ex (T.ESEQ (seq [(cond) (thenLabel, elseLabel),
                                     T.LABEL thenLabel,
                                     T.MOVE (T.TEMP r, unEx thenExp),
                                     T.JUMP (T.NAME joinLabel, [joinLabel]),
                                     T.LABEL elseLabel,
                                     T.MOVE (T.TEMP r, unEx elseExp),
                                     T.LABEL joinLabel],
                                T.TEMP r))
                  | (_, Nx _, Nx _) =>
                    Nx (seq [(cond) (thenLabel, elseLabel),
                             T.LABEL thenLabel,
                             unNx thenExp,
                             T.JUMP (T.NAME joinLabel, [joinLabel]),
                             T.LABEL elseLabel,
                             unNx elseExp,
                             T.LABEL joinLabel])
                  | (_,_,_) => raise Impossible("ifBody and elseBody must be same type") (* We shouldn't get here ever *)          
      
    end
      
    fun intExp (i) = Ex (T.CONST (i)) (* Return a constant of that value *)
    fun nilExp () = Ex (T.CONST (0))
    
    fun stringExp str =
        let
          val label = Temp.newlabel()
        in
          frags := Frame.STRING (label, str) :: !frags;
          Ex (T.NAME label)
        end
      
    fun assignExp (var, exp) =
      let
        val var = unEx var
        val exp = unEx exp
      in
        Nx (T.MOVE (var, exp))
      end
    
    fun breakExp break = Nx (T.JUMP(T.NAME break, [break]))
    
    fun binopExp (oper, left, right) = Ex(T.BINOP(oper, unEx left, unEx right))
    
    fun relopExp (oper, left, right) = 
      Cx(fn(t, f) => T.CJUMP(oper, unEx left , unEx right , t, f))
      
    fun relopStrExp (oper, left, right, str) = 
      Ex (Frame.externalCall (str, unEx left, unEx right))
                                            
    fun intOpExp (A.PlusOp, left, right)   = binopExp (T.PLUS, left, right)
      | intOpExp (A.MinusOp, left, right)  = binopExp (T.MINUS, left, right)
      | intOpExp (A.TimesOp, left, right)  = binopExp (T.MUL, left, right)
      | intOpExp (A.DivideOp, left, right) = binopExp (T.DIV, left, right)
      | intOpExp (A.EqOp, left, right)     = relopExp (T.EQ, left, right)
      | intOpExp (A.NeqOp, left, right)    = relopExp (T.NE, left, right)
      | intOpExp (A.LtOp, left, right)     = relopExp (T.LT, left, right)
      | intOpExp (A.LeOp, left, right)     = relopExp (T.LE, left, right)
      | intOpExp (A.GtOp, left, right)     = relopExp (T.GT, left, right)
      | intOpExp (A.GeOp, left, right)     = relopExp (T.GE, left, right)
                                            
    fun stringOpExp (A.EqOp, left, right)     = relopStrExp (T.EQ, left, right, "stringEqual")
      | stringOpExp (A.NeqOp, left, right)    = relopStrExp (T.NE, left, right, "stringNotEqual")
      | stringOpExp (A.LtOp, left, right)     = relopStrExp (T.LT, left, right, "stringLessThan")
      | stringOpExp (A.LeOp, left, right)     = relopStrExp (T.LE, left, right, "stringLessThanOrEqual")
      | stringOpExp (A.GtOp, left, right)     = relopStrExp (T.GT, left, right, "stringGreaterThan")
      | stringOpExp (A.GeOp, left, right)     = relopStrExp (T.GE, left, right, "stringGreaterThanEqual")      
      | stringOpExp (_, _, _)                 = raise Impossible ("illegal operation on strings")
    fun callExp (_:level, label, exps:exp list) = Ex(T.CALL(T.NAME(label), map unEx exps))       
  
    fun letExp ([], body) = body
      | letExp (decs, body) = Ex (T.ESEQ (seq (map unNx decs), unEx body))
    
    fun seqExp(exps) =   
      let
        val firsts = List.rev(List.tl(List.rev exps))
        val last = List.last exps
      in
        Ex(T.ESEQ(seq(map unNx firsts),unEx last))
      end
    
    fun leveleq (Level(_,uref1),Level(_,uref2)) = uref1=uref2
      | leveleq (_,_) = false

    fun traceSL (deflevel,curlevel as Level({frame:Frame.frame, parent:level}, _)) = 
      if leveleq(deflevel,curlevel) then
        T.TEMP(Frame.FP)
      else
        let 
          val locals = !(#locals frame)
        in
          T.MEM(T.BINOP(T.PLUS, T.CONST(locals), traceSL(deflevel, parent)))
        end
    | traceSL (_,TopLevel) = T.TEMP(Frame.FP)
    
    fun simpleVar(acc,l) = 
      let
        val deflevel = (#1 acc)
        val curacc = (#2 acc)
        fun getAcc(Top, _) = 
              raise Impossible ("can't declare a local variable in global scope")
          | getAcc(Level({frame=frame, parent=parentl}, _), frameacc) = 
              Frame.exp(frameacc)(traceSL(deflevel, l))
      in
        Ex(getAcc(acc))
      end
    
    fun fieldVar (var, offset) =
      let
        val var = unEx var
        val addr = Temp.newtemp ()
      in
          Ex(T.ESEQ(T.MOVE(T.TEMP(addr),
            T.BINOP(T.PLUS,var,
            T.BINOP(T.MUL,unEx offset,
            T.CONST(Frame.wordsize)))),
            T.MEM(T.TEMP(addr))))
        end
    
    fun subscriptExp(arr, offset) =
        let
          val address = Temp.newtemp()
          val arr = unEx arr
          val offset = unEx offset
        in
            Ex(T.ESEQ(
              T.MOVE(T.TEMP(address),
              T.BINOP(T.PLUS,
              arr,
              T.BINOP(T.MUL,
              offset,
              T.CONST(Frame.wordsize)))),
              T.MEM(T.TEMP(address))))
          end
          
    fun recordExp (explist) = hd(explist) (*todo*)
    
    fun procEntryExit({level=level, body=exp})= 
      let
        val frame = (case level of
                      Level({frame,parent}, _) => frame
                    | Top => raise ErrorMsg.Error)

        val body' = Frame.procEntryExit1(frame, unNx(exp))
        val frag = Frame.PROC({body=body',frame=frame})
        val _ = (frags := frag::(!frags))
      in
        ()
      end
    fun getResult() = !frags
    
end