(* Page 224 *)
signature MAKEGRAPH = 
sig
    val instrs2graph : Assem.instr list ->
                        Flow.flowgraph * Flow.Graph.node list
end

structure Makegraph :> MAKEGRAPH =
struct
  structure G = Flow.Graph
  structure A = Assem    
  
  fun instrs2graph instrs = 
  let
    val g = G.newGraph()
    val nodelist = []
    val emptyT = Graph.Table.empty
    fun initInstr([]) = {}
      | initInstr(inst_h::inst_t) = 
      
      let
        val {control, def, use, ismove} = initInstr(inst_t) (* Do for each instruction *)   
        val node = G.newNode(g)
      in
        (
          nodelist := nodelist @ node;
          (* OPER, LABEL, MOVE *) 
          (case inst_h of 
            A.OPER {assem,dst,src,jump} =>
              {
                instn = (G.Table.Enter (instn, node, inst_h)),
                def = (G.Table.Enter (def, node, dst)),
                use = (G.Table.Enter (use, node, src)),
                ismove = (G.Table.Enter (use, node, false)),       
              }
      
            | A.LABEL {assem, label} =>
              {
                instn = (G.Table.Enter (instn, node, inst_h)),
                def = (G.Table.Enter (def, node, [])),
                use = (G.Table.Enter (use, node, [])),
                ismove = (G.Table.Enter (use, node, false)),
              }
              
            | A.MOVE {assem,dst,src} =>
              {
                instn = (G.Table.Enter (instn, node, inst_h)),
                def =  (G.Table.Enter (def, node, [dst])),
                use =  (G.Table.Enter (use, node, [src])),
                ismove = (G.Table.Enter (use, node, true))
              }
          )
        )
      end
      
      fun makeEdges (instn, a::(b::c)) =
        let
          (* Get each instrucion *)
          val inst = G.Table.look(instn, a)
        in
         (* Make edge for follow through *) 
          G.mk_edge {from=a, to=b}
          (* Check for a jump instr *)
          ( case inst of SOME (A.OPER {assem, dst, src, jump}) =>
            (case jump of SOME labellist =>
                       app ( fn label => mk_edge(a, label2node (label))) labellist
                | NONE => ()
            )
            | NONE => () 
            | SOME(_) => ())
            makeEdges(instn, (b::c))
          
        end
      
        | makeEdges (_) = ()
        
        fun label2node (instn, a::b) =
          let
            val inst = G.Table.look(instn, a)
          in
            (case inst of SOME (A.LABEL {assem, lab}) => a
              | NONE => ()
              | SOME(_) => label2node (b)
            )
          end
            

      val {instn, def, use, ismove} = initInstr(instrs)
  in
    (
      makeEdges(instn, nodelist);
      (FGRAPH {g, def, use, ismove}, nodelist)
    )
  end
end