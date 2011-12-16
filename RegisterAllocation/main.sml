structure Main = struct

  structure Tr = Translate
  structure Frame : FRAME = MipsFrame

  (*structure R = RegAlloc*)

  fun getsome (SOME x) = x

  fun emitproc out (Frame.PROC{body,frame}) =
    let 
      (*val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")*)
      (*val _ = Printtree.printtree(out,body); *)
      val stms = Canon.linearize body
      (*val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      val instrs = List.concat(map (MipsGen.codegen frame) stms') 
      val (_, allocation) = RegAlloc.alloc(instrs, frame)
      val format0 = Assem.format(Temp.makestring)
    in  
      app (fn i => TextIO.output(out,format0 i)) instrs
    end
  | emitproc out (Frame.STRING(lab,s)) = 
      TextIO.output(out, Frame.string(Temp.namedlabel(Symbol.name lab), s))

  fun withOpenFile fname f = 
    let 
      val out = TextIO.openOut fname
    in 
      (f out before TextIO.closeOut out) 
      handle e => (TextIO.closeOut out; raise e)
    end 

  fun compile (infile, outfile) = 
    let 
      val absyn = Parse.parse infile
      val frags = ((*FindEscape.prog absyn;*) Semant.transProg absyn)
    in 
      withOpenFile (outfile) 
      (fn out => (app (emitproc out) (#exp frags)))
    end

end



