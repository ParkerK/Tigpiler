structure Types =
struct

  type unique = unit ref

  datatype ty = RECORD of (Symbol.symbol * ty) list * unique
              | NIL
              | INT
              | STRING
              | ARRAY of ty * unique
              | NAME of Symbol.symbol * ty option ref
              | UNIT
  fun toString ty = 
    (case ty of 
       NIL => "NIL"
     | INT => "INT"
     | STRING => "STRING"
     | ARRAY(ty, uq) => "ARRAY[" ^ (toString ty) ^ "]"
     | RECORD(symtys, uq) =>
       "RECORD{" ^ (String.concatWith ","
          (map (fn (sym,ty) => 
             (Symbol.name(sym) ^ ":" ^ (toString ty)))
               symtys)) ^ "}"
     | NAME(sym, tyref) => "NAME(" ^ Symbol.name(sym) ^ ")"
     | UNIT => "UNIT")
end
