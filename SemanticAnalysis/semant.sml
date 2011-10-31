signature SEMANT =
sig
    val transProg : Absyn.exp -> unit
end 

structure Semant :> SEMANT = struct
  structure A = Absyn
  structure E = Env
  val err = ErrorMsg.error
  exception ErrMsg


  val nestLevel = ref 0

  fun tlookup tenv name pos = 
    (case Symbol.look(tenv,name) of
	 NONE => (err pos ("unbound type name "); Types.UNIT)
       | SOME t => t)
       
  fun transTy tenv t = 
    let fun ttyl [] = []
	  | ttyl ({name,escape,typ,pos} :: fl) = 
	    (name,tlookup tenv typ pos) :: ttyl fl
    in (case t of
	    A.NameTy (n,pos) => tlookup tenv n pos
          | A.RecordTy fl => Types.RECORD (ttyl fl,ref())
          | A.ArrayTy (n,pos) => Types.ARRAY (tlookup tenv n pos, ref()))
    end
    
    fun compare_ty (ty1, ty2, pos)=
      (case !ty1 = !ty2 of
          true => ()
          | false => err pos "type mismatch")
    
    fun actual_ty (Types.NAME (s,ty)) = 
        (case !ty of
            NONE => raise ErrMsg
            | SOME t => actual_ty t)
            | actual_ty t = t 

    fun checkInt ({exp, ty}, pos) =
      ((case ty of
          Types.INT => ()
          | _ => err pos "integer required");
       exp)

    fun checkUnit ({exp, ty}, pos) =
      ((case ty of
          Types.UNIT => ()
          | _ => err pos "unit required");
       exp)

    fun checkString ({exp, ty}, pos) =
         ((case ty of
             Types.STRING => ()
             | _ => err pos "string required");
          exp)


     (* Takes venv, tenv, exp *)
    fun transExp(venv, tenv)  =      

      let fun trexp (A.NilExp)    =    {exp=(), ty=Types.NIL}
          | trexp   (A.IntExp i)  =    {exp=(), ty=Types.INT}
          | trexp   (A.StringExp (str, pos)) = {exp=(), ty=Types.STRING}

          | trexp   (A.OpExp {left, oper, right, pos}) = 
              if oper = A.PlusOp orelse oper = A.MinusOp
                 orelse oper = A.TimesOp orelse oper = A.DivideOp then               

                (
                 checkInt(trexp left, pos);
                 checkInt(trexp right, pos);
                 {exp=(), ty=Types.INT}
                )

              else if oper = A.EqOp orelse oper = A.NeqOp orelse oper = A.LtOp
                      orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.GeOp then
                      
                let
                    val left' = trexp left
                    val right' = trexp right
                in
                    (case #ty left' of
                        Types.INT =>
                          (checkInt(left', pos);
                          checkInt(right', pos);
                          {exp=(), ty=Types.INT})

                        | Types.STRING =>
                          (checkString(left', pos);
                          checkString(right', pos);
                          {exp=(), ty=Types.INT})
                         
                         | _ => (err pos "can't perform comparisons on this type";
                                {exp=(), ty=Types.INT}))
                        (*| _ => err pos ("cannot peform comparisons on type"^#ty left') )*)
                 end
             else
                (err pos "error";{exp=(), ty=Types.INT})

        | trexp   (A.CallExp {func, args, pos}) = 
            (case Symbol.look (venv, func) of
                NONE => (err pos "can't call nonexistant functions"; {exp=(), ty=Types.UNIT})
                | SOME (E.FunEntry {formals, result}) =>
                (*if
                    length(args') <> length(args)
                then (err pos "wrong amount of arguments";    {exp=(), ty=result})
                else*)
                {exp=(), ty=Types.UNIT}
                )

                (* Should check to make sure return types match, as do argtypes *)

        | trexp   (A.IfExp {test, then', else', pos}) =
             (case else' of
                 NONE =>
                 let
                     val test' = trexp (test)
                     val then'' = trexp (then')
                 in
                     (checkInt (test', pos);
                     checkUnit (then'', pos);
                     {exp=(), ty=Types.UNIT})
                 end
                 | SOME else' =>
                 let
                     val test' = trexp (test)
                     val then'' = trexp (then')
                     val else'' = trexp (else')                    
                 in
                     checkInt(test', pos)
                     checkUnit(then'', pos)
                     checkUnit(else'', pos)
                     {exp=(), ty=Types.UNIT}
                 end
              )

          | trexp   (A.WhileExp {test, body, pos}) =
              let
                  (*nestLevel := !nestLevel + 1*)
                  val body' = transExp (tenv,venv) body
                  (*nestLevel := !nestLevel - 1*)
              in
                  checkInt (test, pos)
                  checkUnit (body', pos)
                  {exp=(), ty=Types.UNIT}
              end   

          | trexp   (A.RecordExp {fields, typ, pos}) =
              (case Symbol.look (tenv, typ) of
                 SOME (record as Types.RECORD (fields, _)) => 
                 {exp=(), ty=typ}
                 (* Should check types *)
                 | NONE => (err pos "no record found"))

          | trexp   (A.SeqExp exps) =
              {exp=(), ty=Types.UNIT}

          | trexp   (A.AssignExp {var, exp, pos}) =
              let
                  val  {exp=left,  ty=expect} = transExp (var)
                  val  {exp=right, ty=actual} = transExp (venv, tenv, exp)
              in
                  if
                    expect <> actual
                  then
                    err pos "assignment mismatch"
                  else
                    {exp=(), ty=Types.UNIT}
              end

          | trexp   (A.ForExp {var, escape, lo, hi, body, pos}) =
                
                checkInt(transExp (lo), pos)
                checkInt(transExp (hi), pos)
                (* Add Stuff Here *)
                {exp=(),ty=Types.UNIT}
            

          | trexp   (A.BreakExp pos) =
              if !nestLevel > 0 
              then
                  {exp=(), ty=Types.UNIT}
              else
                  (err pos "Break not nested correctly";
                      {exp=(), ty=Types.UNIT})



          | trexp   (A.LetExp {decs, body, pos}) =
              let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, decs)
                  in transExp(venv', tenv') body
              end

          | trexp   (A.ArrayExp {typ, size, init, pos}) =
              {exp=(), ty=Types.UNIT}

         and trvar (A.SimpleVar(id,pos)) = 
                      (case Symbol.look(venv, id) of
                          SOME (E.VarEntry{ty}) =>
                              {exp=(), ty=actual_ty ty}
                          | NONE => (err pos "undefined variable: ";
                              {exp=(), ty=Types.INT})
                             )

          | trvar (A.FieldVar(var,id,pos)) =
              let
                  val var' = transExp (var)
              in
                  (case var' of
                      {exp, ty=record as Types.RECORD (fields, _)} =>
                      {exp=(), ty=record}
                      | _ => err pos "no var found"
                      )
              end

          | trvar (A.SubscriptVar(var, exp,pos)) =

              checkInt(trexp exp, pos)
              {exp=(), ty=Types.UNIT}

      in
        trexp
      end
      
      and transDec (venv, tenv, A.VarDec{name, typ=NONE, init,... }) = 
          let val {exp,ty} = transExp(venv, tenv, init)
              in {tenv = tenv,
                  venv=Symbol.enter(venv, name, E.VarEntry{ty=ty})}
              end

          | transDec (venv, tenv, A.TypeDec[{name, ty}]) = 
              {venv = venv,
                tenv=Symbol.enter(tenv, name, transTy(tenv, ty))}

          | transDec(venv, tenv, A.FunctionDec[{name, params, body,
                                              pos, result=SOME(rt,pos)}]) =
              let val SOME(result_ty) = Symbol.look(tenv, rt)
                  fun transparam {name, typ, pos} = 
                      case Symbol.look(tenv, typ)
                          of SOME t => {name=name typ=t}
                  val params' = map transparam params
                  val venv' = Symbol.enter(venv, name, 
                              E.FunEntry{formals = map #ty params', result = result_ty})
                  fun enterparam ({name, ty}, venv) = 
                      Symbol.enter (venv, name,
                              E.VarEntry{access=(), ty=ty})
                  val venv'' = fold enterparam params' venv'
              in transExp(venv'', tenv) body;
                  {venv=venv', tenv=tenv}
              end
    
    
    fun transProg(absyn) = 
        let in transExp(venv, tenv) end
end