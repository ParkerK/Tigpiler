signature FRAME =
sig
  type frame
  type access

  type register = string

  val FP : Temp.temp
  val ZERO : Temp.temp
  val RA : Temp.temp  
  val RV : Temp.temp (*as seen by callee*)
  val registers : register list
  val tempMap : register Temp.Table.table

  val wordsize : int
  val argregs: Temp.temp list
  val calldefs : Temp.temp list
  val callersaves : Temp.temp list
  val colorable : Temp.temp list
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  
  val string : Tree.label * string -> string
  
  val exp : access -> Tree.exp -> Tree.exp
  
  val externalCall : string * Tree.exp list -> Tree.exp
  
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
                
end