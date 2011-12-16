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

end