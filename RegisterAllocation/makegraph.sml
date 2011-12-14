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
    val g = G.newGraph()
    val nodelist = []
    
    fun initInstr([]) =
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
                control = g,
                def = (G.Table.Enter (def, node, dst)),
                use = (G.Table.Enter (use, node, src)),
                ismove = (G.Table.Enter (use, node, false)),       
              }
      
            | A.LABEL {assem, label} =>
              {
                control = g,
                def = (G.Table.Enter (def, node, nil)),
                use = (G.Table.Enter (use, node, nil)),
                ismove = (G.Table.Enter (use, node, false)),
              }
              
            | A.MOVE {assem,dst,src} =>
              {
                control = g,
                def =  (G.Table.Enter (def, net, dst)),
                use =  (G.Table.Enter (use, node, src)),
                ismove = (G.Table.Enter (use, node, true))
              }
          )
        )
      end
      
      fun makeEdges (control, a::(b::c)) =
        let
          (* Get each instrucion *)
          val inst = G.Table.look(control, a)
        in
         (* Make edge for follow through *) 
          G.mk_edge {from=a, to=b}
          (* Check for a jump instr *)
          ( case inst of SOME (A.OPER {assem, dst, src, jump}) =>
            (case jump of SOME labellist => label2node (label)
                | NONE => ())
            | NONE => () 
            | SOME(_) => ())
            makeEdges(control, (b::c))
          
        end
      
        | makeEdges (_) = ()
        
        fun label2node (control, a::b) =
          let
            val inst = G.Table.look(control, a)
          in
            (case inst of SOME (A.LABEL {assem, lab}) => a
              | NONE => ()
              | SOME(_) => label2node (b)
            )
          end
            

      val {control, def, use, ismove} = initInstr(instrs)
  in
    (
      makeEdges(control, nodelist);
      (FGRAPH{control, def, use, ismove}, nodelist)
    )
  end
end