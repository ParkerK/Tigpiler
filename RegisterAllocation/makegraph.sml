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
    val emptyT = Graph.Table.empty
    fun initInstr([]) = 
          ({
            instn = emptyT,
            def = emptyT,
            use = emptyT,
            ismove = emptyT       
          }, [])
      | initInstr(inst_h::inst_t) = 
          let
            (* Do for each instruction *)
            val ({instn, def, use, ismove},nodelist) = initInstr(inst_t)
            val node = G.newNode(g)
          in
            (
              (* OPER, LABEL, MOVE *) 
              (case inst_h of 
                A.OPER {assem,dst,src,jump} =>
                  {
                    instn = (G.Table.enter (instn, node, inst_h)),
                    def = (G.Table.enter (def, node, dst)),
                    use = (G.Table.enter (use, node, src)),
                    ismove = (G.Table.enter (ismove, node, false))       
                  }
      
                | A.LABEL {assem, lab} =>
                  {
                    instn = (G.Table.enter (instn, node, inst_h)),
                    def = (G.Table.enter (def, node, [])),
                    use = (G.Table.enter (use, node, [])),
                    ismove = (G.Table.enter (ismove, node, false))
                  }
              
                | A.MOVE {assem,dst,src} =>
                  {
                    instn = (G.Table.enter (instn, node, inst_h)),
                    def =  (G.Table.enter (def, node, [dst])),
                    use =  (G.Table.enter (use, node, [src])),
                    ismove = (G.Table.enter (ismove, node, true))
                  }
              ), 
              nodelist @ [node]
            )
          end
      
      val ({instn, def, use, ismove}, nodelist) = initInstr(instrs)
      
      fun makeEdges (instn, a::(b::c)) =
        let
          (* Get each instrucion *)
          val inst = G.Table.look(instn, a)
        in
         (* Make edge for follow through *) 
          (G.mk_edge {from=a, to=b};
          (* Check for a jump instr *)
            (case inst of 
              SOME (A.OPER {assem, dst, src, jump}) =>
              (case jump of 
                SOME labellist =>
                      app (fn label => G.mk_edge({from=a, to=label2node(instn, nodelist, label)})) labellist
                | NONE => () )
            | SOME(_) => ()
            | NONE => ()
            );
            makeEdges(instn, (b::c));
            ())
        end
      
        | makeEdges (_) = ()
        
      and label2node (instn, a::b, lbl): G.node =
        let
          val inst = G.Table.look(instn, a)
        in
          (case inst of SOME (A.LABEL {assem, lab}) =>
              (if lbl = lab then a else label2node (instn, b, lbl))
            | SOME(_) => label2node (instn, b, lbl)
            | NONE => ErrorMsg.impossible ("can't find label!")
          )
        end
       | label2node(_) = ErrorMsg.impossible ("can't find label!")

      val _ = makeEdges(instn, nodelist)
  in
    (Flow.FGRAPH {control=g, def=def, use=use, ismove=ismove}, nodelist)
  end
end