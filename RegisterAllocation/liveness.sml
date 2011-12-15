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
                      
      val liveins = map (fn _ => []) nodelist
      val liveouts = map (fn _ => []) nodelist
      
      fun live(node, ins : Temp.temp list, outs : Temp.temp list) =
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
                          tempSet.listItems(tempSet.union (makeSet(list), makeSet(List.nth(liveins, index))))
                      | NONE => list
                    ) [] sucNodes
            end
        
        in
          (ins', outs')
        end
      
      fun calcLive(inlist: Temp.temp list list, outlist: Temp.temp list list) =
        let
          val inouts1 = ListPair.zip(inlist, outlist)
          val oldlist: (G.node* (Temp.temp list* Temp.temp list)) list = ListPair.zip(nodelist, inouts1)
          
          val newstuff = List.foldr (fn ((n, (iii, ooo)), list) => (n, live(n, iii, ooo))::list)
                                [] oldlist
          val (n, inouts) = ListPair.unzip(newstuff)
          val (newinlist, newoutlist) = ListPair.unzip(inouts)
          
          fun listcomp(i1, i2) = List.all (fn (a, b) => a=b) (ListPair.zip(i1, i2))
          fun listlistcomp(l1, l2) = List.all (fn (i1, i2) => listcomp(i1, i2)) (ListPair.zip(l1,l2))
          val stop = (listlistcomp(inlist, newinlist) andalso listlistcomp(outlist, newoutlist))
        in
          if stop then
            (newinlist, newoutlist)
          else
            calcLive(newinlist, newoutlist)
        end
            
      fun fillMappings() = 
        let
          val (inl, outl) = calcLive(liveins, liveouts)
          val l1 = ListPair.zip(nodelist, inl)
          val l2 = ListPair.zip(nodelist, outl)
          val fnodemap = foldr (fn ((n,l),t)=> G.Table.enter(t, n, l)) G.Table.empty l2
          val livemap = foldr (fn ((n,l),t)=> G.Table.enter(t, n, makeLiveSet(l))) G.Table.empty l1
        in
          (fnodemap, livemap)
        end
        
      val (fnodeToTemps, globalLiveMap) = fillMappings()
    
      val igraph = G.newGraph()
      val moves = []
      val templist = foldr (fn (node, list) => 
        list @ tempSet.listItems(tempSet.union(makeSet(getList(def, node)), 
                                               makeSet(getList(use, node))))
      ) [] nodelist
      val (tnode, gtemp) = foldr (fn (temp, (temptonode, nodetotemp)) => 
                let 
                  val inode = G.newNode(igraph)
                in
                  (Temp.Table.enter(temptonode, temp, inode),
                   G.Table.enter(nodetotemp, inode, temp))
                end
              ) 
              (Temp.Table.empty : G.node Temp.Table.table,
               G.Table.empty : Temp.temp G.Table.table)
              templist
    in
      (
        IGRAPH {
                graph = igraph, 
                tnode = fn temp => 
                          case Temp.Table.look(tnode, temp) of
                            SOME n => n
                          | NONE => ErrorMsg.impossible ("can't find temp"), 
                gtemp = fn node => 
                          case G.Table.look(gtemp, node) of
                            SOME t => t
                          | NONE => ErrorMsg.impossible ("can't find node"),
                moves = []
                },
        fn n => case G.Table.look(fnodeToTemps, n) of 
                  SOME (l) => l
                | NONE => ErrorMsg.impossible ("can't find label!")
      )
    end
  
        
  fun show (outstream, igraph) = ()
  
end