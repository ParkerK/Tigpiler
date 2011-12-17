(* Page 253 *)
signature COLOR = 
sig
  structure Frame : FRAME
  
  type allocation = Frame.register Temp.Table.table
  
  val color : {interference: Liveness.igraph,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
              -> allocation * Temp.temp list
                  
end

structure Color :> COLOR =
struct
  
  structure G = Liveness.G
  structure Frame : FRAME = MipsFrame
  
  type allocation = Frame.register Temp.Table.table
  val didSpill = ref false
  structure Set = ListSetFn(struct
                            type ord_key = Temp.temp
                            val compare  = Int.compare
                            end)
                            
 							
  fun spill() = (print "Spilling!"; didSpill := true)
  fun color {interference, initial : allocation, spillCost : Graph.node -> int, registers : Frame.register list} = 
  let
    val Liveness.IGRAPH {graph, tnode, gtemp, moves} = interference
    val nodes = G.nodes (graph)
    val nodeSet = Set.addList(Set.empty, (map gtemp (nodes)))
	val spillSet = Set.addList(Set.empty, (map gtemp (nodes))) (* If we spill, we spill EVERYTHING *)
    val colorPalette = Set.addList(Set.empty, Frame.colorable)
    val cTable = ref Temp.Table.empty
    val coloredNodes = ref Set.empty
  	val regColorMap = ref initial : allocation ref
    val validcolors = ref colorPalette
    
                
    fun numColorables () = Set.numItems(colorPalette)
                
    fun neighbors(node) = List.length(G.adj(node))
  	fun temp2reg(temp) = valOf(Temp.Table.look(Frame.tempMap,temp))
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
          (G.rm_edge{from=n, to=node}; popPred(nt, node)) 
        | popPred([], _) = ()
    and popSucc (n::nt, node) = 
          (G.rm_edge{from=node, to=n}; popSucc(nt, node))
        | popSucc([], _) = ()
    
    fun sortNodes (nodes) = (* Make sure we're passing this a SET! *)
    (* filter f se
    creates a new set containing only those elements of se that satisfy the predicate f. This is equivalent to:
    
    List.foldl add' empty (List.filter f (listItems se))   *)
    
    let
      val lt = (fn (n) => (neighbors(tnode n) < numColorables ()  ))
      val gte = (fn (n) => (neighbors (tnode n) >= numColorables () ))
    in
      ( 
        (Set.filter lt nodes),
        (Set.filter gte nodes)
      )
    end
    
    fun colorNode (node) =
      let
        val neighbors = G.adj (node)      
        fun rmC([]) = ()
        | rmC(n::ns) = (
          (if Set.member((!coloredNodes),n) then
            (print "deleting \n";(validcolors := Set.delete((!validcolors),valOf(Temp.Table.look((!cTable),n)))))
          else ());
          rmC(ns))
      
      in (
        print ("Validcolors : " ^(Int.toString (List.length (Set.listItems(!validcolors)))) ^ "\n");
        if Set.isEmpty((!validcolors)) then
          (spill())
        else
          let
            val color = hd((Set.listItems((!validcolors))))
            val node' = gtemp node
          in (
            print ("Colored Nodes : " ^(Int.toString (List.length (Set.listItems(!coloredNodes)))) ^ "\n");
            (*print ("Coloring: " ^ (Int.toString(node')) ^ temp2reg(color) ^"\n");*)
            coloredNodes := Set.add((!coloredNodes),  node');
            regColorMap := Temp.Table.enter((!regColorMap),  node', temp2reg(color));
            cTable := Temp.Table.enter((!cTable),  node', color));
            rmC([node'])
          end
          )
      end
      
    fun colorTable (ltk, gtk) =
      let 
        fun isEmpty (s) = Set.isEmpty(s)
      in (
        if (isEmpty ltk andalso isEmpty gtk)
          then () (* done *)
        else (
          if (isEmpty ltk ) then 
            spill() (* we'll need to spill registers *)
          else (
            let 
              val head = hd(Set.listItems(ltk))
              val node = tnode head
              val _ = colorNode node 
              val _ = popNode node
              val remaining = Set.union(ltk, gtk)
              val remaining = Set.delete(remaining, head)
              val (newltk, newgtk) = sortNodes(remaining)
            in
              (*print (Int.toString (List.length (Set.listItems (newltk))) ^ "\n");*)
              colorTable(newltk,newgtk)
            end )
            )
          )
        end
        
  val (under_k, above_k) = sortNodes (nodeSet) 
  val _ =  colorTable (under_k, above_k)
  val returnSpills = [];
  
  in
	 if !didSpill then returnSpills = [] else returnSpills = Set.listItems(spillSet);
    (!regColorMap,returnSpills)    
  end 
  
end