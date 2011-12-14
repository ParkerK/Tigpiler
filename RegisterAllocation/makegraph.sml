(* Page 224 *)
signature MAKEGRAPH = 
sig
    val instrs2graph : Assem.instr list ->
                        Flow.flowgraph * Flow.Graph.node list
end

structure Makegraph :> MAKEGRAPH =
struct
  structure G = FLOW.Graph
  structure A = Assem    
  
  fun instrs2graph instrs = 
  
  
  let
    val base = G.newGraph()
    
    fun initInstr([]) = 
      | initInstr(inst_h::inst_t) = 
      
      let
        val {control, def, use, ismove} = initInstr(inst_t) (* Do for each instruction *)   
        val next = G.newNode(base) 
      in
        (* OPER, LABEL, MOVE *) 
        (case inst_h of 
          A.OPER {assem,dst,src,jump=NONE} =>
            {
              control = (G.Table.Enter (control, next, inst_h)),
              def = (G.Table.Enter (def, next, dst)),
              use = (G.Table.Enter (use, next, src)),
              ismove = (G.Table.Enter (use, next, false)),         
            }
      
          | A.LABEL {assem, label} =>
            {
              control = (G.Table.Enter (control, next, inst_h)),
              def = (G.Table.Enter (def, next, nil)),
              use = (G.Table.Enter (use, next, nil)),
              ismove = (G.Table.Enter (use, next, false)),
            }

          |A.MOVE {assem,dst,src} =>
            {
              control = (G.Table.Enter (control, next, inst_h)),
              def =  (G.Table.Enter (def, net, dst)),
              use =  (G.Table.Enter (use, next, src)),
              ismove = (G.Table.Enter (use, next, true))
            }
          )
    
      end
      
      fun makeEdges (a::(b::c)) =
        
        let
          val node = G.look(control, a)
        in
          G.mk_edge {from=a, to=b}
        end
      
        | makeEdges (_) = ()
  
end