(* Page 253 *)
signature REG_ALLOC = 
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame ->
                    Assem.instr list * allocation
                  
end

structure RegAlloc :> REG_ALLOC = 
struct
  structure Frame : FRAME = MipsFrame
  fun alloc(instrs, frame) = 
    let
      val (fgraph, nodelist) = Makegraph.instrs2graph(instrs)
      val (igraph, liveoutmapping) = Liveness.interferenceGraph(fgraph, nodelist)
      (*val _ = Liveness.show(out, igraph)*)
      val allocation = Frame.tempMap
      val reglist = Frame.registers
      val (newalloc, templist) = 
        Color.color {interference = igraph,
                    initial = allocation,
                    spillCost = (fn node => 0),
                    registers = reglist}
    in
      (instrs, newalloc)
    end
end