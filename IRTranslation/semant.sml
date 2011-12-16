signature SEMANT =
sig
  (*val transProg : Absyn.exp -> {exp:Translate.frag list, ty:Types.ty}*)
  val transProg : Absyn.exp -> Tree.stm
end 

structure Semant :> SEMANT = struct
  structure A = Absyn
  structure E = Env
  structure Tr = Translate
  val err = ErrorMsg.error
  exception ErrMsg

  val nestLevel = ref 0
  
  fun incLevel () = nestLevel := !nestLevel + 1
  fun decLevel () = nestLevel := !nestLevel - 1
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
            (case SOME(typelookup tenv typ pos) of 
               SOME t => (name, t)
             | NONE => (name, Types.UNIT))) fields
      fun checkdups(h::l) = 
            (List.exists (fn {name, escape, typ, pos}=> 
                if (#name h)=name then
                  (err pos ("duplicate field: " ^ Symbol.name name);
                  true)
                else
                  false) l;
            checkdups(l))
        | checkdups(_) = ()
      in
        case t of
          A.NameTy (n, pos) => typelookup tenv n pos
        | A.RecordTy fields => (checkdups(fields);Types.RECORD (recordtys fields, ref()))
        | A.ArrayTy (n,pos) => Types.ARRAY(typelookup tenv n pos, ref())
      end
  
  fun compare_ty (ty1, ty2, pos)=
    case ty1 of 
      Types.RECORD(_,_) => (ty1 = ty2) orelse ty2 = Types.NIL
    | Types.NIL => (ty1 = ty2) orelse (case ty2 of Types.RECORD(_,_) => true
                                        |  _ => (ty2 = Types.NIL))
    | _ => (err pos "type mismatch"; false)

  fun compare_tys ([], trexps, pos) = {exp=Tr.empty, ty=Types.UNIT}
      | compare_tys(tys, [], pos) = {exp=Tr.empty, ty=Types.UNIT}
      | compare_tys(t1::l1,t2::l2,pos) = (compare_ty(t1,t2,pos); compare_tys(l1,l2,pos))
  
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
  fun transExp(venv, tenv, break, level)  = (*removed break to make things compile*)

    let 
      fun trexp (A.NilExp) = {exp=Tr.nilExp(), ty=Types.NIL}
      | trexp (A.VarExp var) = trvar var
      | trexp (A.IntExp i) = {exp=(Tr.intExp(i)), ty=Types.INT}
      | trexp (A.StringExp (str, pos)) = {exp=Tr.stringExp(str), ty=Types.STRING}
      | trexp (A.OpExp {left, oper, right, pos}) = 
        if oper = A.PlusOp orelse oper = A.MinusOp orelse 
           oper = A.TimesOp orelse oper = A.DivideOp then
          let 
            val left' = trexp left
            val right' = trexp right
          in
          (checkInt(left', pos);
           checkInt(right', pos);
           {exp=Tr.intOpExp(oper, #exp left', #exp right'), ty=Types.INT})
         end
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
                {exp=Tr.intOpExp(oper, #exp left', #exp right'), ty=Types.INT})
            | Types.STRING =>
                (checkString(left', pos);
                checkString(right', pos);
                {exp=Tr.stringOpExp(oper, #exp left', #exp right'), ty=Types.INT})
            | _ => (err pos "can't perform comparisons on this type";
                  {exp=Tr.nilExp(), ty=Types.INT}))
           end
       else
        (err pos "error";{exp=Tr.nilExp(), ty=Types.INT})

    | trexp   (A.CallExp {func, args, pos}) = 
      (case Symbol.look (venv, func) of
        SOME (E.FunEntry {formals, result, level, label}) =>
          let
            val args' = map trexp args
          in
            (if
              length(args) <> length(formals) (* check argument lengths and compare *)
            then 
              (err pos "wrong amount of arguments"; {exp=Tr.nilExp(), ty=result})
            else
              (compare_tys (formals, map #ty args', pos);
              {exp=Tr.callExp(level,label,map #exp (map trexp args)),ty=actual_ty result})
            )
          end
          
        | _ => (err pos ("can't call nonexistant function: " ^ Symbol.name func ); {exp=Tr.nilExp(), ty=Types.UNIT}))
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
           {exp=(Tr.ifThenExp(#exp test', #exp then'')), ty=Types.UNIT})
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
           {exp=(Tr.ifThenElseExp(#exp test', #exp then'', #exp else'')), ty=Types.UNIT}
         end
        )

      | trexp (A.WhileExp {test, body, pos}) =
        let
          val _ = incLevel()
          val body' = transExp (venv,tenv,break,level) body
          val test' = transExp (venv,tenv,break,level) body
          val _ = decLevel()
        
          val test'' = checkInt (test', pos);
          val body'' = checkUnit (body', pos);
        in
          {exp=Tr.whileExp(#exp test', #exp body', break), ty=Types.UNIT}
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
                        {exp=Tr.recordExp(map #exp tyfields), ty=result} 
                    else 
                        (err pos ("field types not consistent: " ^ Symbol.name typ);
                        {exp=Tr.nilExp(),ty=result})
                  else
                    (err pos ("field types not consistent: " ^ Symbol.name typ);
                    {exp=Tr.nilExp(),ty=result})
                end
            | _ => (err pos ("not a valid record type: " ^ Symbol.name typ);
                    {exp=Tr.nilExp(), ty=Types.UNIT})
        end
        
      | trexp (A.SeqExp []) = {exp=Tr.empty, ty=Types.UNIT}
      | trexp (A.SeqExp exps) =
        let
             val (exps', ty) =
               foldl (fn ((exp, _), (exps', _)) =>
                       let
                         val {exp=newExp, ty} = (transExp(venv, tenv, break, level) exp)
                       in
                         (exps' @ [newExp], ty)
                       end)
                     ([], Types.UNIT)
                exps
        in
             {exp=Tr.seqExp exps', ty=ty}
        end
         
      | trexp (A.AssignExp {var, exp, pos}) =
        let
          val  {exp=left,  ty=expect} = trvar (var)
          val  {exp=right, ty=actual} = trexp (exp)
        in
          if
          expect <> actual
          then
          (err pos "assignment mismatch";{exp=Tr.nilExp(), ty=Types.UNIT})
          else
          {exp=Tr.assignExp(left, right), ty=Types.UNIT}
        end

      | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
        
      let
        val access = Tr.allocLocal level (!escape)
        val breakpoint = Tr.newbreakpoint()
        val {exp=lo', ty=loty} = transExp (venv, tenv, break, level) lo
        val {exp=hi', ty=hity} = transExp (venv, tenv, break, level) hi
        val venv' = Symbol.enter (venv, var, Env.VarEntry {access=access, ty=Types.INT})
        val {exp=bodyExp, ty=body_ty} = transExp (venv', tenv, breakpoint, level) body
      in
        {exp=Tr.forExp(Tr.simpleVar (access, level), breakpoint, lo', hi', bodyExp), ty=Types.UNIT}
      end

      | trexp (A.BreakExp pos) =
        if !nestLevel > 0 
        then
          {exp=Tr.breakExp(break), ty=Types.UNIT}
        else
          (err pos "Break not nested correctly";
            {exp=Tr.nilExp(), ty=Types.UNIT})

      | trexp (A.LetExp {decs, body, pos}) =
          let
            val ({tenv=tenv', venv=venv'}, decList) = transDecs(venv,tenv,decs, break, [], level)
            val _ = print ("--let exp, before "^Int.toString(List.length(decs))^" decs \n")
            val _ = print ("--let exp, after "^Int.toString(List.length(decList))^" decs \n")
            val {exp=bodyExp, ty=bodyTy} = transExp (venv',tenv', break, level) body
          in
            {exp=Tr.letExp(decList,bodyExp), ty=bodyTy}
          end

      | trexp (A.ArrayExp {typ, size, init, pos}) =
        {exp=Tr.nilExp(), ty=Types.UNIT}

     and trvar (A.SimpleVar(id,pos)) = 
          (case Symbol.look(venv, id) of
            SOME (E.VarEntry{access, ty}) => {exp=Tr.simpleVar(access, level), ty=actual_ty ty}
          | _ => (err pos ("undefined variable: " ^ Symbol.name id); {exp=Tr.nilExp(), ty=Types.INT}))

      | trvar (A.FieldVar(var,id,pos)) =
          let
            val var' = trvar var
          in
            (case var' of
              {exp, ty=record as Types.RECORD (fields, _)} => {exp=Tr.nilExp(), ty=record}
            | _ => (err pos "no var found"; {exp=Tr.nilExp(), ty=Types.UNIT}))
          end

      | trvar (A.SubscriptVar(var, exp,pos)) =
          (checkInt(trexp exp, pos);
          {exp=Tr.nilExp(), ty=Types.UNIT})
    in
      trexp
    end
    
    and transDec (venv, tenv, A.VarDec{name,escape=escape, typ=typ, init, pos=pos1}, break, explist, level) =
        let
          val {exp, ty} = transExp (venv, tenv, break, level) init 
          val access = Tr.allocLocal level (!escape)
          val explist' =  explist @ [Tr.assign (Tr.simpleVar (access, level), exp)]
          val _ = case typ of 
                    SOME(s, pos) =>
                      (case Symbol.look (tenv,s) of
                          NONE => (err pos ("type not defined: " ^ Symbol.name s))
                        | SOME ty2 => (compare_ty(ty, ty2, pos1);()))
                  | NONE => ()
        in
          ({tenv = tenv, venv=Symbol.enter(venv, name, E.VarEntry{access=access, ty=ty})}, explist', level)
        end

    | transDec (venv, tenv, A.TypeDec vardecs, break, explist, level) = 
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
                ({tenv=tenv', venv=venv}, explist, level)
            end

    | transDec(venv, tenv, A.FunctionDec(fundecs), break, explist, level) =
      let
        val fundef = ref []
        val levels = ref []

        fun makeheader (fundec:A.fundec,env) = 
          let
              val r_ty = 
                (case (#result fundec) of
                NONE => Types.UNIT
                | SOME(rt,pos) => typelookup tenv rt pos)

              val params' = (map (fn ({name,escape,typ,pos}) => {name=name,
                ty=typelookup tenv typ pos})
                (#params fundec))
                
              val formals = (map (fn ({name,escape,typ,pos}) => (!escape)) (#params fundec))
              val funlabel = Temp.newlabel()
              val new_level = Tr.newLevel{parent=level,
                name=funlabel,
                formals=formals}
              val _ = levels := new_level::(!levels)

            in
              Symbol.enter(env,
                (#name fundec),
                E.FunEntry({formals=(map #ty params'),
                result=r_ty,
                label=funlabel,
                level=new_level}))
        end

        fun enterparam (param:A.field,venv) =
          let
            val access = Tr.allocLocal (hd(!levels)) (!(#escape param))
            val ty = typelookup tenv (#typ param) (#pos param)
          in
            Symbol.enter(venv, (#name param), E.VarEntry{access=access,ty=ty})
          end

        val venv' = (foldl makeheader venv fundecs)
        
        fun check (fundec:A.fundec) = 
          let 
            val venv'' = (foldl enterparam venv' (#params fundec))
            val {exp, ty} = transExp(venv'',tenv,break,level) (#body fundec)
            val check_level = hd ((!levels))
            val _ = (levels := tl ((!levels)))
            val _ = Tr.procEntryExit({level=check_level, body=exp})
          in
            fundef := (!fundef)@[exp]
          end

        val checkres = (map check fundecs)
        val fundef' = !fundef
        val explist' = (explist@fundef')
        
        in
          ({venv=venv',tenv=tenv}, explist', level)
        end
        
    and transDecs (venv, tenv, decs, break, explist, level) =
      (*foldr (fn (dec, ({venv=v, tenv=t}, e, l)) => 
        let val ({venv=venv', tenv=tenv'}, explist', level') =transDec(v, t, dec, break, e, l)
        in 
          ({venv=venv', tenv=tenv'}, explist')
        end
      )
      ({venv=venv, tenv=tenv}, explist, level) decs*)
    (case decs of
          [] => ({venv=venv, tenv=tenv}, explist)
        | (d::ds) => let 
                      val ({venv=venv', tenv=tenv'}, explist', level') = transDec(venv, tenv, d, break, explist, level) (*NONE = break?*)
                    in
                      transDecs(venv', tenv', ds, break, explist', level)
                    end)
    
    fun transProg(exp) =
      let
        val firstlevel = Translate.newLevel {
          name=Temp.namedlabel "firstlevel",
          parent=Tr.outermost,
          formals=[]}

        val {exp, ty} = transExp (Env.base_venv, Env.base_tenv, Temp.newlabel(), firstlevel) exp
      in
        (Tr.procEntryExit {level=firstlevel, body=exp};
        (*{exp=Tr.getResult(), ty=t};*)
        Tr.unNx(exp))
      end
    end