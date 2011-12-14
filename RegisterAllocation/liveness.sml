structure LIVENESS =
sig
  datatype igraph = IGRAPH of {graph: IGraph.graph, 
                               tnode: Temp.temp -> IGraph.node,
                               gtemp: IGraph.node -> Temp.temp,
                               moves: (IGraph.node * IGraph.node) list}
  
  val interferenceGraph : Flow.flowgraph * Flow.Graph.node list -> 
                            igraph * (Flow.Graph.node -> Temp.temp list)
  
  val show : outstream * igraph -> unit
end
structure Liveness : LIVENESS =
struct
  datatype igraph = IGRAPH of {graph: IGraph.graph, 
                               tnode: Temp.temp -> IGraph.node,
                               gtemp: IGraph.node -> Temp.temp,
                               moves: (IGraph.node * IGraph.node) list}

                (* node to *)
  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet Flow.Graph.Table.table
  
  fun interferenceGraph ({control, def, use, ismove}, node::nodelist) = 
    let
      val igraph = Graph.newGraph()
      val tnode = IGraph.node Temp.Table.table
      val gtemp = Temp.temp Temp.Table.table
      val moves = []
      fun livein(node)
        
      fun liveout(node)
    in
      
    end
  
        
  fun show (outstream * igraph) = ()
  
end