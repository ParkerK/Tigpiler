(* Page 253 *)
signature COLOR = 
sig
  structure Frame : FRAME
  
  type allocation = Frame.register Temp.Table.table
  
  val color : {intereference: Liveness.igraph,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
              -> allocation * Temp.temp list
                  
end

structure Color :> COLOR =
struct
  
  structure G = Liveness.Graph
  structure IGraph = Liveess.IGRAPH
  structure F = MipsFrame
  
  type allocation = Frame.register Temp.Table.table
  
  structure Set = ListSetFn(struct
                            type ord_key = Temp.temp
                            val compare  = Int.compare
                            end)
  
  fun color {Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillost, registers} =
  let
    val nodes = G.nodes (graph)
    val nodeSet = Set.addList(Set.empty, (map gtemp (nodes)))
    
    fun neighbors(node) = List.length(G.adj(nodes))
    
    fun popNode(node) =
    let
      val preds = G.pred(node)
      val succs = G.succ(node)
    in
      (
      popPred(preds, node);
      popSucc(succs, node)
      )
    end
    
    and popPred (n::nt, node) =
        (G.rm_egde{from=n, to=node}; popEdge(nt, node)) 
    | popPred([], _) = ()
    
    and popSucc (n::nt, node) = 
    (G.rm_edge{from=node, to=n}; popSucc(nt, node))
    | popSucc([], _) = ()
    
    
  in
    body
  end 
  
end