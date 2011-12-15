signature LIVENESS =
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

  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet Flow.Graph.Table.table
  structure G = FLOW.Graph
  (*http://www.smlnj.org/doc/smlnj-lib/Manual/list-set-fn.html*)
  structure tempSet = ListSetFn(struct
                                  type ord_key = Temp.temp
                                  val compare  = Int.compare
                                  end)  
  fun interferenceGraph ({control, def, use, ismove}, nodelist) = 
    let
      val igraph = Graph.newGraph()
      val tnode = IGraph.node Temp.Table.table
      val gtemp = Temp.temp Graph.Table.table
      val fnodeToTemps = Temp.temp list Graph.Table.table
      
      val globalliveMap = G.Table.empty : liveSet G.Table.table
      val moves = []
      
      fun makeSet(SOME(list)) = tempSet.addList(tempSet.empty, list)
        | makeSet (NONE) = tempSet.empty
      
      fun makeLiveSet(livetemplist) = 
            let
              val tempTable = Temp.Table.empty
              val tempList = []
            in
              app (fn livetemp =>
                    (Temp.Table.enter(tempTable, temp, ());
                    tempList := tempList @ livetemp)
                  ) livetemplist;
              (tempTable, tempList)
            end
        | makeLiveSet() = (Temp.Table.empty, [])
        
      fun livein(node) =
      let
        val usedTemps = G.Table.look(use, node)
        val defTemps = G.Table.look(def, node)
        val outTemps = liveout(node)
      in 
        (case usedTemps of NONE => ()
          | SOME (temp list) =>
            (tempSet.listItems(
              tempSet.union(
                makeSet(usedTemps),
                tempSet.difference( makeSet(outTemps), makeSet(defTemps) )
              )))
          )
      end
      
      and fun liveout(node) = 
      let
        val outTemps = []
        val sucTemps = G.succ(control, node)
      in
        (
          app (fn suc =>
            (app (fn outtemp => 
                    if (List.exists (fn item => G.eq(item, outtemp)) outTemps) then ()
                      else outTemps := outTemps @ outtemp) 
                  livein(suc)))
            sucTemps;
          outTemps
        )
      end
    in
      (
        app (fn node => 
              G.Table.enter(fnodeToTemps, node, liveout(node);
              G.Table.enter(globalliveMap, node, makeLiveSet(livein(node))) nodelist;
        
      )
    end
  
        
  fun show (outstream * igraph) = ()
  
end