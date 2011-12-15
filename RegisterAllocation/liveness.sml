signature LIVENESS =
sig
  structure G : GRAPH
  datatype igraph = IGRAPH of {graph: G.graph, 
                               tnode: Temp.temp -> G.node,
                               gtemp: G.node -> Temp.temp,
                               moves: (G.node * G.node) list}
  
  val interferenceGraph : Flow.flowgraph * G.node list -> 
                            igraph * (G.node -> Temp.temp list)
  
  val show : TextIO.outstream * igraph -> unit
end
structure Liveness : LIVENESS =
struct
  structure G = Flow.Graph
  datatype igraph = IGRAPH of {graph: G.graph, 
                               tnode: Temp.temp -> G.node,
                               gtemp: G.node -> Temp.temp,
                               moves: (G.node * G.node) list}

  type liveSet = unit Temp.Table.table * Temp.temp list
  type liveMap = liveSet G.Table.table
  (*http://www.smlnj.org/doc/smlnj-lib/Manual/list-set-fn.html*)
  structure tempSet = ListSetFn(struct
                                  type ord_key = Temp.temp
                                  val compare  = Int.compare
                                  end)  
  fun interferenceGraph ({control, def, use, ismove}, nodelist) = 
    let
      val igraph = G.newGraph()
      val tnode = Temp.Table.empty : G.node Temp.Table.table
      val gtemp = G.Table.empty : Temp.temp G.Table.table
      val fnodeToTemps = G.Table.empty : Temp.temp list G.Table.table
      
      val globalliveMap = G.Table.empty : liveSet G.Table.table
      val moves = []
      
      fun makeSet(list : Temp.temp list) = tempSet.addList(tempSet.empty, list)
        | makeSet (_) = tempSet.empty
      
      fun makeLiveSet(livetemplist : Temp.temp list) = 
            let
              val tempTable = Temp.Table.empty
              val tempList = []
            in
              app (fn livetemp =>
                    (Temp.Table.enter(tempTable, livetemp, ());
                    tempList := tempList @ [livetemp])
                  ) livetemplist;
              (tempTable, tempList)
            end
        | makeLiveSet(_) = (Temp.Table.empty, [])
        
      fun livein(node) : Temp.temp list =
      let
        val usedTemps = case G.Table.look(use, node) of 
                          SOME templist => templist
                        | NONE => []
        val defTemps = case G.Table.look(def, node) of 
                          SOME templist => templist
                        | NONE => []
        val outTemps = liveout(node)
      in 
        tempSet.listItems(
          tempSet.union(
            makeSet(usedTemps),
            tempSet.difference( makeSet(outTemps), makeSet(defTemps) )
          ))
      end
      
      and liveout(node) : Temp.temp list = 
      let
        val outTemps = []
        val sucTemps = G.succ(node)
      in
        (
          app (fn suc =>
            (app (fn outtemp => 
                    if (List.exists (fn item => G.eq(item, outtemp)) outTemps) then ()
                      else outTemps := outTemps @ [outtemp]) 
                  (livein(suc))))
            sucTemps;
          case outTemps of 
            [] => NONE
          | _ => SOME(outTemps)
        )
      end
    in
      (
        app (fn node => 
              (
                fnodeToTemps := G.Table.enter(fnodeToTemps, node, liveout(node));
                globalliveMap := G.Table.enter(globalliveMap, node, makeLiveSet(livein(node)))
              )
            ) nodelist;
        IGRAPH {
                graph = igraph, 
                tnode = fn _ => Graph.newNode(igraph), 
                gtemp = fn _ => Temp.newtemp(),
                moves = []
                }
      )
    end
  
        
  fun show (outstream, igraph) = ()
  
end