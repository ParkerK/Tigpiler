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
  fun interferenceGraph ({control, def, use, ismove} : Flow.flowgraph, nodelist:G.node list) = 
    let
      val igraph = G.newGraph()
      val tnode = Temp.Table.empty : G.node Temp.Table.table
      val gtemp = G.Table.empty : Temp.temp G.Table.table
      val moves = []
      
      fun makeSet(list : Temp.temp list) = tempSet.addList(tempSet.empty, list)
        | makeSet (_) = tempSet.empty
      
      fun makeLiveSet(_) = (Temp.Table.empty, [])
        | makeLiveSet(livetemplist : Temp.temp list) = 
            let
              fun genLiveSet(ttbl, tlist, []) = (ttbl, tlist)
                | genLiveSet(ttbl, tlist, temp::templist) =
                let
                  val ttbl' = Temp.Table.enter(ttbl, temp, ())
                  val tlist' = temp::tlist
                in
                  genLiveSet(ttbl', tlist', templist)
                end
            in
              genLiveSet(Temp.Table.empty, [], livetemplist)
            end
        
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
          val sucTemps = G.succ(node)
          
          fun genLiveOut(outlist, []) = outlist
            | genLiveOut(outlist, suc::suclist) =
                let
                  val outlist' = tempSet.listItems(tempSet.union (makeSet(outlist), makeSet(livein(suc))))
                in
                  genLiveOut(outlist', suclist)
                end
        in
          genLiveOut([], sucTemps)
        end
        
      fun fillMappings(fnodemap, livemap, []) = (fnodemap, livemap)
        | fillMappings(fnodemap, livemap, n::nlist) = 
            let
              val fnodemap' = G.Table.enter(fnodemap, n, liveout(n))
              val livemap' = G.Table.enter(livemap, n, makeLiveSet(livein(n)))
            in
              fillMappings(fnodemap', livemap', nlist)
            end
        
      val (fnodeToTemps, globalLiveMap) = 
        fillMappings(G.Table.empty : Temp.temp list G.Table.table,
                     G.Table.empty : liveSet G.Table.table,
                     nodelist)
  
    in
      (
        IGRAPH {
                graph = igraph, 
                tnode = fn _ => Graph.newNode(igraph), 
                gtemp = fn _ => Temp.newtemp(),
                moves = []
                },
        fn _ => []
      )
    end
  
        
  fun show (outstream, igraph) = ()
  
end