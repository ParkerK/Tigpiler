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
  type level = int ref
  val outermost = ref 0
  
  fun newLevel({parent=l, name=n, formals=f}) = (Frame.newFrame({name=n, formals=true::f}); ref(!l+1)) (*change level?*)
  fun formals(l:level) = let val fs = tl(Frame.formals()) (*remove the true value for static link*)
                       fun convertAccess([]) = []
                         | convertAccess(h::accs) = (l, h) :: convertAccess(accs)
                   in
                       convertAccess(fs)
                   end
                     
  fun allocLocal(l:level) = fn(b) => (l, Frame.allocLocal(f??)(b))
  
  exception Impossible of string
  
  fun simpleVar(acc, l) = Ex(T.CONST 0) (*TODO*)

  fun seq([]) = T.LABEL(Temp.newlabel())
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
      | ifExp () (* TODO *)
      
    end
      
    fun int (i) = Ex (T.CONST (i)) (* Return a constant of that value *)
    
    fun string (str) =
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
      
    
  
end