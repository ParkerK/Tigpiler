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
  structure Frame = MipsFrame
  structure T = Tree
  datatype exp  = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm
  type access = level * Frame.access
  type level = int ref
  val outermost = ref 0
  
  fun newLevel({parent: l, name: n, formals: f}) = (Frame.newFrame(n, true::f); !l+1) (*change level?*)
  fun formals(l) = let val fs = drop(Frame.formals(frame?), 1) (*remove the true value for static link*)
                       fun convertAccess([]) = []
                         | convertAccess(h::accs) = (l * h) @ convertAccess(accs)
                   in
                       convertAccess(fs)
                   end
                     
  fun allocLocal(l) = fn(b) => (l * Frame.allocLocal(f??)(b))
  
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
end