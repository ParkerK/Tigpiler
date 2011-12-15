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
  fun interferenceGraph (Flow.FGRAPH {control, def, use, ismove}, nodelist:G.node list) = 
    let
      val igraph = G.newGraph()
      val tnode = Temp.Table.empty : G.node Temp.Table.table
      val gtemp = G.Table.empty : Temp.temp G.Table.table
      val moves = []
      
      fun makeSet(list : Temp.temp list) = tempSet.addList(tempSet.empty, list)
      
      fun makeLiveSet(livetemplist : Temp.temp list) = 
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

      fun getList(table, node) = case G.Table.look(table, node) of 
                        SOME templist => templist
                      | NONE => []
      fun live(node, ins, outs) =
        let
          val defTemps = getList(def, node)
          val usedTemps = getList(use, node)
          val ins' = tempSet.listItems(
            tempSet.union(
              makeSet(usedTemps),
              tempSet.difference( makeSet(outs), makeSet(defTemps) )
            ))
          
          val sucNodes = G.succ(node)
          val outs' = 
            let 
              fun find(n, list, baseindex) =
                if baseindex >= List.length(list) then NONE
                else
                  if G.eq(n, List.nth(nodelist, baseindex)) then SOME(baseindex)
                  else find(n, list, baseindex+1)
            in
              foldr (fn (sucnode, list) => 
                      case find(sucnode, nodelist, 0) of
                        SOME index => 
                          tempSet.listItems(tempSet.union (makeSet(list), makeSet(List.nth(ins, index))))
                      | NONE => list
                    ) sucNodes []
            end
        
        in
          (ins', outs')
        end
      
      val liveins = map (fn _ => []) nodelist
      val liveouts = map (fn _ => []) nodelist
      
      fun calcLive(inlist, outlist) =
        let
          val newstuff = foldr (fn (n, ins, outs) => (n, live(n, ins, outs))) 
                                [] List.zip(nodelist, List.zip(inlist, outlist))
          val (_,(newinlist, newoutlist)) = List.unzip(List.unzip(newstuff))
          fun listlistcomp(l1, l2) = List.all (fn (i1, i2) => listcomp(i1, i2)) List.zip(l1,l2)
          fun listcomp(i1, i2) = List.all (fn (a, b) => a=b) List.zip(i1, i2)
          val continue = (listlistcomp(inlist, newinlist) andalso listlistcomp(outlist, newoutlist))
        in
          if continue then
            calcLive(newinlist, newoutlist)
          else
            (newinlist, newoutlist)
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