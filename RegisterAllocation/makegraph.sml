(* Page 224 *)
signature MAKEGRAPH = 
sig
    val instrs2graph : Assem.instr list ->
                        Flow.flowgraph * Flow.Graph.node list
end

structure MAKEGRAPH :> MAKEGRAPH =
struct
  structure G = FLOW.Graph    
  
  (*
  Input : A sequence of instructions (mostly Three address code).
  Output: A list of basic blocks with each three-address statement in exactly one block.
  Step 1. Identify the leaders in the code. Leaders are instructions which come under any
    of the following 3 categories :
  The first instruction is a leader.
  The target of a conditional or an unconditional goto/jump instruction is a leader.
  The instruction that immediately follows a conditional or an unconditional goto/jump
    instruction is a leader.
  Step 2. Starting from a leader, the set of all following instructions until and not
    including the next leader is the basic block corresponding to the starting leader.
  *)
  
  fun instrs2graph instrs = 
  
  
  let
    val base = G.newGraph()
    
  in
    
  end
  
end