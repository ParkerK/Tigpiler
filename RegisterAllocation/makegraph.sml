(* Page 224 *)
signature MAKEGRAPH = 
sig
    val instrs2graph : Assem.instr list ->
                        Flow.flowgraph * Flow.Graph.node list
end

structure MAKEGRAPH :> MAKEGRAPH =
struct
  structure G = FLOW.Graph    
  
  fun instrs2graph instrs = 
  
  
  let
    val base = G.newGraph()
    
    fun initInstr([]) = 
      | initInstr(inst_h::inst_t) = 
      
    
  in
    
  end
  
end