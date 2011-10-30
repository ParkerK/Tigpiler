signature SEMANT =
sig
  type ty
  type S
  val tenv : ty S.table
  val venv : enventry S.table
  val expty = {exp: Translate.exp, ty: ty}
  val transProg : Absyn.exp -> unit
  val transVar : venv * tenv * Absyn.var -> expty
  val transExp : venv * tenv * Absyn.exp -> expty
  val transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy :         tenv * Absyn.ty  -> ty
end

structure Semant :> SEMANT =
  type ty = Type.ty
  structure A = Absyn
  structure S = Symbol
  structure E = Env
  
 fun transProg(absyn) = 
    let in transExp(venv, tenv) end

  val nestLevel = ref 0

    fun compare_ty (ty1, ty2, pos)=
      (case !ty1 = !ty2 of
          true => ()
          | false => err pos "type mismatch")
    fun actual_ty (T.NAME(var, ty) , pos)=
      ( case !ty of 
          NONE => (E.error pos "Undefined type"; Types.INT)
          | SOME t => actual_ty (t, pos))
          | actual_ty (t,pos)= t 
      )

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

    fun checkType ({exp, ty}, pos, type) =
      ((case ty of
        type => ()
        | _ => err pos "unit required");
     exp)

     (* Takes venv, tenv, exp *)
    fun transExp(venv, tenv, exp)  =      

      let fun trexp (A.NilExp)    =    {exp=Translate.Nil(), ty=Types.NIL}
          | trexp   (A.IntExp i)  =    {exp=Translate.Int(int), ty=Types.INT}
          | trexp   (A.StringExp (str, pos) = {exp=Translate.String(str), ty=Types.STRING}

          | trexp   (A.OpExp {left, oper, right, pos}) = 
              if oper = A.PlusOp orelse oper = A.MinusOp
                 orelse oper = A.TimesOp orelse oper = A.DivideOp                  

                (
                 checkInt(trexp left, pos);
                 checkInt(trexp right, pos);
                 {exp=()}, ty=Types.INT}
                )

              else if oper = A.EqOp orelse oper = A.NeqOp orelse oper = A.LtOp
                      orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.GeOp

                let
                    left' = trexp left
                    right' = trexp right
                in
                    (case left' of
                        Types.INT =>
                          (checkInt(left', pos);
                          checkInt(right', pos);
                          {exp=()}, ty=Types.INT})

                        | Types.STRING = >
                          (checkString(left', pos);
                          checkString(right', pos);
                          {exp=()}, ty=Types.INT})

                        | _ => err pos "cannot peform comparisons on type" #ty left' )
                 end

        | trexp   (A.CallExp {func, args, pos}) = 
            (case S.look (venv, func) of
                NONE => (err pos "can't call nonexistant functions")
                | SOME (E.FunEntry {formals, result})

                if length(formals) <> length(args) then err pos "wrong amount of arguments"
                (* Should check to make sure return types match, as do argtypes *)
                {exp=()}, ty=result}

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
                  val break = Translate (*set new break*)
                  nestLevel := !nestLevel + 1
                  val body' = transExp (tenv,venv,lev,break) body
                  nestLevel := !nestLevel - 1
              in
                  checkInt (test, pos)
                  checkUnit (body', pos)
                  {exp=(), ty=Types.UNIT}
              end   

          | trexp   (A.RecordExp {fields, typ, pos}) =
              (case S.look (tenv, typ) of
                 SOME (record as Types.RECORD (fields, _)) => 
                 {exp=(), ty=typ}
                 (* Should check types *)
                 | NONE = > (err pos "no record found")
                 end     

          | trexp   (A.SeqExp exps) =
              {exp=(), ty=Types.UNIT}

          | trexp   (A.AssignExp {var, exp, pos}) =
              let
                  val  {exp=left,  ty=expect} = transVar (var)
                  val  {exp=right, ty=actual} = transExp (venv, tenv, exp)
              in
                  if expect <> actual then err pos "assignment mismatch"
                  {exp=(), ty=Types.UNIT}
              end

          | trexp   (A.ForExp {var, escape, lo, hi, body, pos}) =
              let
                  checkInt(transExp (lo), pos)
                  checkInt(transExp (hi), pos)
                  (* Add Stuff Here *)

              in
                  {exp=(),ty=Types.UNIT}
              end

          | trexp   (A.BreakExp pos) =
              if !nestLevel > 0 
              then
                  {exp=(), ty=Types.UNIT}
              else
                  (err pos "Break not nested correctly";
                      {exp=(), ty=Types.UNIT})



          | trexp   (A.LetExp {decs, body, pos}) =
              let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
                  in transExp(venv', tenv') body
              end

          | trexp   (A.ArrayExp {typ, size, init, pos}) =
              {exp=(), ty=Types.UNIT}

         and trvar (A.SimpleVar(id,pos)) = 
                      (case S.look(venv, id) of
                          SOME (E.VarEntry{ty}) =>
                              {exp=(), ty=actual_ty ty}
                          | NONE => (err pos ("undefined variable: " ^ S.name id)
                              exp(), ty=Types.INT))

          | trvar (A.FieldVar(var,id,pos)) =
              let
                  var' = transExp (var)
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
        trexp(exp)
      end
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