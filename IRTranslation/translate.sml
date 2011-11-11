signature TRANSLATE = 
sig
  type access (*not the same as Frame.access*)
  type level 
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access

  val simpleVar : access * level -> exp
  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)
end

structure Translate : TRANSLATE = struct 
  structure Frame : FRAME = MipsFrame
  structure T = Tree
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  type access = level * Frame.access
  datatype level =  Top
                  | Level of {frame:Frame.frame, parent: level} * unit ref
  val outermost = Top
  
  fun newLevel({parent=l, name=n, formals=f}) = (Level ({frame=Frame.newFrame {name=n, formals=true::f}, parent=l}, ref())) (*change level?*)
  fun formals(Top) = []
    | formals(Level({frame, parent}, uref)) =
        let val fs = tl(Frame.formals(frame)) (*remove the true value for static link*)
             fun convertAccess([]) = []
               | convertAccess(h::accs) = (l, h) :: convertAccess(accs)
         in
             convertAccess(fs)
         end

 fun allocLocal(Top) = []
   | allocLocal(Level({frame, parent}, uref) l) = fn(b) => (l,Frame.allocLocal(frame)(b))
  
  exception Impossible of string
  
  fun simpleVar(acc, l) = Ex(T.CONST 0) (*TODO*)

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
  
  fun unCx(Ex e) = (fn (t, f) => T.CJUMP(T.NE, e, T.CONST 0, t, f))
    | unCx(Cx genstm) = genstm
    | unCx(Nx _) = raise Impossible("Cannot unCx an Nx")
    
    
  fun whileExp (test, body, done) = 
    let
      val test = unCx test
      val body = unNx body
      val testLabel = Temp.newlabel()
      val bodyLabel = Temp.newlabel() 
    in 
      Nx (seq [T.LABEL testLabel, 
          test (bodyLabel, done),
          T.LABEL bodyLable,
          body,
          T.JUMP (T.NAME testLabel, [testLabel]),
          T.LABEL done])
    end
    
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
    
    fun ifExp (T.CONST _, thenExp, _) = thenExp
      | ifExp (T.CONST 0, _, elseExp) = elseExp
      | ifExp (cond, thenExp, elseExp) =
        let
          val cond = unCx cond
          val thenLabel = Temp.newlabel()
          val elseLabel = Temp.newlabel()
          val joinLabel = Temp.newlabel()
          val r = Temp.newTemp() (* Only for when thenExp/elseExp = Ex, suggested on page 162 *)
        in
          case (cond, thenExp, elseExp) of
                    (T.CONST 1, _, _) => thenExp
                  | (T.CONST 0, _, _) => elseExp
                  | (_, Cx _, Cx _) =>
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
    fun stringExp (str) = 
      let
        val strLabel = Temp.newlabel()
      in
        (* Frame call to handle string *)
      end
      
    fun assignExp (var, exp) =
      let
        val var = unEx var
        val exp = unEx exp
      in
        Nx (T.MOVE (var, exp))
      end
    
    fun breakExp break = Nx (T.JUMP(T.NAME break, [break]))
      
    fun intOpExp (A.PlusOp)   = BINOP T.PLUS
      | intOpExp (A.MinusOp)  = BINOP T.MINUS
      | intOpExp (A.TimesOp)  = BINOP T.MUL
      | intOpExp (A.DivideOp) = BINOP T.DIV
      | intOpExp (A.EqOp)     = RELOP T.EQ
      | intOpExp (A.NeqOp)    = RELOP T.NE
      | intOpExp (A.LtOp)     = RELOP T.LT
      | intOpExp (A.LeOp)     = RELOP T.LE
      | intOpExp (A.GtOp)     = RELOP T.GT
      | intOpExp (A.GeOp)     = RELOP T.GE
      
    fun callExp() (* TODO *)
  
end