(* Page 253 *)
signature REG_ALLOC = 
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame ->
                    Assem.instr list * allocation
                  
end

structure RegAlloc :> REGALLOC = 
struct

  fun alloc(list, frame) = 
    let
      val (fgraph, nodelist) = Makegraph.instrs2graph(instrs)
      val (igraph, liveoutmapping) = Liveness.interferenceGraph(fgraph, nodelist)
      (*val _ = Liveness.show(out, igraph)*)
      
    in
    end
end