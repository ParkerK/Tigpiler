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
      
      
  structure A = Absyn      
  
  val nestLevel = ref 0
  
  fun actual_ty (T.NAME(var, ty) , pos)=
    ( case !ty of 
        NONE => (E.error pos "Undefined type"; Types.INT)
        | SOME t => actualTy (t, pos))
        | actual_ty (t,pos)= t 
    )
    
    fun checkInt ({exp, ty}, pos) =
    ((case ty of
        Types.INT => ()
        | _ => err pos "integer required");
     exp)
     
  fun checkUnit ({exp, ty}, pos) =
    ((case s of
        Types.UNIT => ()
        | _ => err pos "unit required");
     exp)
  
   (* Takes venv, tenv, exp *)
  fun transExp(venv, temv)  =      
    let fun trexp (A.NilExp)    =    {exp=Translate.Nil(), ty=Types.NIL}
        | trexp   (A.IntExp i)  =    {exp=Translate.Int(int), ty=Types.INT}
        | trexp   (A.StringExp (str, pos) = {exp=Translate.String(str), ty=Types.STRING}
        
        | trexp   (A.CallExp {func, args, pos}) = 
            (case Symbol.look (venv, func) of
                NONE => (error)
                | SOME (Env.FunEntry {label, level=})
                let
                    
                in
                    (if length(form))
                
                )
        
        
        | trexp   (A.OpExp {left, oper, right, pos}) = 
            if     oper = A.PlusOp orelse oper = A.MinusOp orelse oper = A.TimesOp orelse oper = A.DivideOp
                   orelse oper = A.EqOp orelse oper = A.NeqOp orelse oper = A.LtOp orelse oper = A.LeOp
                   orelse oper = A.GtOp orelse oper = A.GeOp
                  
                  (checkInt(trexp left, pos);
                   checkInt(trexp right, pos);
                   {exp=()}, ty=Types.INT})

       
        | trexp   (A.RecordExp {fields, typ, pos}) =
        | trexp   (A.SeqExp exps) =
            let
                val (exps', ty) =
                    foldl (fn ((exp, _), (exps', _)) =>
                        let
                            val {exp=exp'', ty} =
                                transExp (venv, tenv, level, bp, exp)
                            in
                                (exps' @ [exp''], ty)
                            end
                        )
                        ([], Types.UNIT)
                        exps
                in
                    {exp=Translate.sequence exps', ty=ty}
                end
                                
        
        | trexp   (A.AssignExp {var, exp, pos}) =
            let
                val  {exp=left,  ty=expect} = transVar (var)
                val  {exp=right, ty=actual} = transExp (venv, tenv, level, bp, exp)
            in
                ()
            
        | trexp   (A.IfExp {test, then', else', pos}) =
            (case else' of
                NONE =>
                let
                    val test' = trexp (test)
                    val then'' = trexp (then')
                in
                    checkInt(test', pos)
                    checkUnit(then'', pos)
                    {exp=(), ty=Types.UNIT}
                end
                | SOME else' =>
                let
                    val test' = trexp (test)
                    val else'' = trexp (else')                    
                in
                    checkInt(test', pos)
                    checkUnit(else'', pos)
                    {exp=(), ty=Types.UNIT}
                end
             )
                
        | trexp   (A.WhileExp {test, body, pos}) =
            let
                val test' = checkInt (test, pos)
                val break = Translate (*set new break*)
                nestLevel := !nestLevel + 1
                val body' = transExp (tenv,venv,lev,break) body
                nestLevel := !nestLevel - 1
                val bodyexp = checkUnit (body', pos)
            in
                {exp=Translate.whileExp(test',body',break), ty=Types.UNIT}
            end
            
        | trexp   (A.ForExp {var, escape, lo, hi, body, pos}) =
        | trexp   (A.BreakExp pos) =
            if !nestLevel > 0 
            then
                {exp=Translate.breakExp(break), ty=Types.UNIT}
            else
                (err pos "Break not nested correctly";
                    {exp=(), ty=Types.UNIT})
            
        
        
        | trexp   (A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
                in transExp(venv', tenv') body
            end
        
        | trexp   (A.ArrayExp {typ, size, init, pos}) =
        
       and trvar (A.SimpleVar(id,pos)) = 
        | (A.FieldVar(var,id,pos))=
        | (A.SubscriptVar(var, exp,pos))
        
        
    fun transDec (venv, tenv, A.VarDec{name, typ=NONE, init, escape, pos}) = 
        let val {exp,ty} = transExp(venv, temv, init)
            in {tenv = tenv,
                venv=S.enter(venv, name, E.VarEntry{ty=ty})}
            end
        
        | transDec (venv, tenv, A.TypeDec[{name, ty}]) = 
            {venv = venv,
            tenv=S.enter(tenv, name, transTy(tenv, ty))}
            
        | transDec(venv, tenv, A.FunctionDec[{name, params, body,
                                            pos, result=SOME(rt,pos)}]) =
            let val SOME(result_ty) = S.look(tenv, rt)
                fun transparam {name, typ, pos} = 
                    case S.look(tenv, typ)
                        of SOME t => {name=namem ty=t}
                val params' = map transparam params
                val venv' = S.enter(venv, name, 
                            E.FunEntry{formals = map #ty params', result = result_ty})
                fun enterparam ({name, ty}, venv) = 
                    S.enter (venv, name,
                            E.VarEntry{access=(), ty=ty})
                val venv'' = fold enterparam params' venv'
            in transExp(venv'', tenv) body;
                {venv=venv', tenv=tenv}
            end
                                              
  

end

    
