structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
      | NAME of Symbol.symbol * ty option ref
      | UNIT
      
      
      
  (* Takes venv, tenv, exp *)
  (*= VarExp of var
          | NilExp
          | IntExp of int
          | StringExp of string * pos
          | CallExp of {func: symbol, args: exp list, pos: pos}
          | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
          | RecordExp of {fields: (symbol * exp * pos) list, typ: symbol, pos: pos}
          | SeqExp of (exp * pos) list
          | AssignExp of {var: var, exp: exp, pos: pos}
          | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
          | WhileExp of {test: exp, body: exp, pos: pos}
          | ForExp of {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp, pos: pos}
          | BreakExp of pos
          | LetExp of {decs: dec list, body: exp, pos: pos}
          | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}
  *)
  fun transExp(venv, temv)  =
    let fun trexp (A.NilExp) = 
        | trexp   (A.IntExp i) = 
        | trexp   (A.StringExp (str, pos) = 
        | trexp   (A.CallExp {func, args, pos}) =
        | trexp   (A.OpExp {left, oper, right, pos}) = 
        | trexp   (A.RecordExp {fields, typ, pos}) =
        | trexp   (A.SeqExp var) =
        | trexp   (A.AssignExp {var, exp, pos}) =
        | trexp   (A.IfExp {test, then', else', pos}) =
        | trexp   (A.WhileExp {test, body, pos}) =
        | trexp   (A.ForExp {var, escape, lo, hi, body, pos}) =
        | trexp   (A.BreakExp pos) =
        | trexp   (A.LetExp {decs, body, pos}) =
        | trexp   (A.ArrayExp {typ, size, init, pos}) =
        
       and trvar (A.SimpleVar(id,pos)) = 
        | (A.FieldVar(var,id,pos))=
        | (A.SubscriptVar(var, exp,pos))
        
        
  

end

    
