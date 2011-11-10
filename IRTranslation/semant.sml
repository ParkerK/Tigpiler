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

  fun typelookup tenv n pos= 
  let 
    val result=Symbol.look (tenv, n)
  in  
    (case result of
      SOME ty2 => ty2
    | NONE => (err pos ("type is not defined: " ^ Symbol.name n) ; Types.UNIT))
  end

  fun transTy (tenv, t)=
    let 
      fun recordtys(fields)= map (fn{name, escape, typ, pos}=>
            (case SOME(typelookup tenv name pos) of 
               SOME typ => (name, typ)
             | NONE => (name, Types.UNIT))) fields
      in
        case t of
          A.NameTy (n, pos) => typelookup tenv n pos
        | A.RecordTy fields => Types.RECORD (recordtys fields, ref())
        | A.ArrayTy (n,pos) => Types.ARRAY(typelookup tenv n pos, ref())
      end
  
  fun compare_ty (ty1, ty2, pos)=
    (case ty1 = ty2 of
      true => true
    | false => (err pos "type mismatch"; false))
  
  fun actual_ty (Types.NAME (s,ty)) = 
    (case !ty of
      NONE => raise ErrMsg
    | SOME t => actual_ty t)
  |   actual_ty t = t 

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

    let fun trexp (A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp (A.VarExp var) = trvar var
      | trexp (A.IntExp i) = {exp=(), ty=Types.INT}
      | trexp (A.StringExp (str, pos)) = {exp=(), ty=Types.STRING}
      | trexp (A.OpExp {left, oper, right, pos}) = 
        if oper = A.PlusOp orelse oper = A.MinusOp orelse 
           oper = A.TimesOp orelse oper = A.DivideOp then
          (checkInt(trexp left, pos);
           checkInt(trexp right, pos);
           {exp=(), ty=Types.INT})
        else if oper = A.EqOp orelse oper = A.NeqOp orelse oper = A.LtOp orelse
                oper = A.LeOp orelse oper = A.GtOp orelse oper = A.GeOp then
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
           end
       else
        (err pos "error";{exp=(), ty=Types.INT})

    | trexp   (A.CallExp {func, args, pos}) = 
      (case Symbol.look (venv, func) of
        SOME (E.FunEntry {formals, result}) =>
          (*if
            length(args') <> length(args)
          then (err pos "wrong amount of arguments";    {exp=(), ty=result})
          else*)
          {exp=(), ty=Types.UNIT}
        | _ => (err pos ("can't call nonexistant function: " ^ Symbol.name func ); {exp=(), ty=Types.UNIT}))
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
           checkInt(test', pos);
           checkUnit(then'', pos);
           checkUnit(else'', pos);
           {exp=(), ty=Types.UNIT}
         end
        )

      | trexp (A.WhileExp {test, body, pos}) =
        let
          (*nestLevel := !nestLevel + 1*)
          val body' = transExp (venv,tenv) body
          val test' = transExp (venv,tenv) body
          (*nestLevel := !nestLevel - 1*)
        
          val test'' = checkInt (test', pos);
          val body'' = checkUnit (body', pos);
        in
          {exp=(), ty=Types.UNIT}
        end 

      | trexp (A.RecordExp {fields, typ, pos}) =
        let 
            val typ' = typelookup tenv typ pos
            val result = actual_ty typ'
            val fnames = map #1 fields
            val tyfields = map trexp (map #2 fields)
            val types = map #ty tyfields
        in case result of
            Types.RECORD(s, u) =>
                let 
                  val dfnames = map #1 s
                  val dftypes = map actual_ty (map #2 s)
                in
                  if fnames = dfnames then
                    if (ListPair.all
                        (fn (ty1, ty2) => compare_ty (ty1, ty2, pos))
                        (types, dftypes))
                    then
                        {exp=(), ty=Types.RECORD(s,u)} 
                    else 
                        (err pos ("field types not consistant: " ^ Symbol.name typ);
                        {exp=(),ty=Types.RECORD(s,u)})
                  else
                    (err pos ("field types not consistant: " ^ Symbol.name typ);
                    {exp=(),ty=Types.RECORD(s,u)})
                end
            | _ => (err pos ("not a valid record type: " ^ Symbol.name typ);
                    {exp=(), ty=Types.UNIT})
        end
        

      | trexp (A.SeqExp exps) =
        {exp=(), ty=Types.UNIT}

      | trexp (A.AssignExp {var, exp, pos}) =
        let
          val  {exp=left,  ty=expect} = trvar (var)
          val  {exp=right, ty=actual} = trexp (exp)
        in
          if
          expect <> actual
          then
          (err pos "assignment mismatch";{exp=(), ty=Types.UNIT})
          else
          {exp=(), ty=Types.UNIT}
        end

      | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
        
        let
          val lo' = checkInt(trexp (lo), pos)
          val hi' = checkInt(trexp (hi), pos)
        (* Add Stuff Here *)
        in
          {exp=(),ty=Types.UNIT}
        end

      | trexp (A.BreakExp pos) =
        if !nestLevel > 0 
        then
          {exp=(), ty=Types.UNIT}
        else
          (err pos "Break not nested correctly";
            {exp=(), ty=Types.UNIT})

      | trexp (A.LetExp {decs, body, pos}) =
        let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
          in transExp(venv', tenv') body
        end

      | trexp (A.ArrayExp {typ, size, init, pos}) =
        {exp=(), ty=Types.UNIT}

     and trvar (A.SimpleVar(id,pos)) = 
          (case Symbol.look(venv, id) of
            SOME (E.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
          | _ => (err pos ("undefined variable: " ^ Symbol.name id); {exp=(), ty=Types.INT}))

      | trvar (A.FieldVar(var,id,pos)) =
          let
            val var' = trvar var
          in
            (case var' of
              {exp, ty=record as Types.RECORD (fields, _)} => {exp=(), ty=record}
            | _ => (err pos "no var found"; {exp=(), ty=Types.UNIT}))
          end

      | trvar (A.SubscriptVar(var, exp,pos)) =
          (checkInt(trexp exp, pos);
          {exp=(), ty=Types.UNIT})
    in
      trexp
    end
    
    and transDec (venv, tenv, A.VarDec{name, typ=NONE, init,... }) = 
          let 
            val {exp,ty} = transExp (venv, tenv) init
          in 
            {tenv = tenv, venv=Symbol.enter(venv, name, E.VarEntry{ty=ty})}
          end

    | transDec (venv, tenv, A.VarDec{name,escape= ref true ,typ=SOME(s, pos), init, pos=pos1}) =
        let
            val {exp, ty} = transExp (venv, tenv) init 
        in
            ( case Symbol.look (tenv,s) of
                NONE => (err pos ("type not defined: " ^ Symbol.name s))
                | SOME ty2=>  if ty<>ty2 then (err pos "type mismatch") else ();
                {tenv=tenv,
                venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty})  } )
        end

    | transDec (venv, tenv, A.TypeDec vardecs) = 
          let
            val names = map #name vardecs
            val poss = map #pos vardecs
            val typs = map #ty vardecs
            fun addt (n,env) = Symbol.enter(env,n,Types.NAME(n,ref(Symbol.look(tenv, n))))
            val tenv' = foldr addt tenv names
            val nts = map (fn t => transTy (tenv', t)) typs
            fun updt (n,nt) = 
              let val (SOME (Types.NAME(_,r))) = Symbol.look(tenv',n)
                  in r := SOME nt
              end
            val _ = app updt (ListPair.zip(names,nts))
            in 
                {tenv=tenv', venv=venv}
            end

    | transDec(venv, tenv, A.FunctionDec[{name, params, body, pos, result=SOME(rt,pos1)}]) =
        let val result_ty = case Symbol.look(tenv, rt) of 
                              SOME(res) => res
                            | NONE => Types.UNIT
          fun transparam {name, escape, typ, pos} = 
            case Symbol.look(tenv, typ) of
               SOME t => {name=name, typ=t}
            |  NONE => (err pos "type undefined"; {name=name, typ=Types.UNIT})
          val params' = map transparam params
          val venv' = Symbol.enter(venv, name, 
              E.FunEntry{formals = map #typ params', result = result_ty})
          fun enterparam ({name, typ}, venv) = 
              Symbol.enter (venv, name, E.VarEntry{ty=typ})
          val venv'' = foldr enterparam venv' params'
        in transExp(venv'', tenv) body;
          {venv=venv', tenv=tenv}
        end
    | transDec(venv, tenv, _) = {venv=venv, tenv=tenv}

    and transDecs (venv, tenv, decs) =
    (case decs of
      [] => {venv=venv, tenv=tenv}
    | (d::ds) => let 
                  val {venv=venv', tenv=tenv'} = transDec(venv, tenv, d)
                in
                  transDecs(venv', tenv', ds)
                end)
    
  fun transProg(absyn) = (transExp (E.base_venv, E.base_tenv) absyn; ())
end