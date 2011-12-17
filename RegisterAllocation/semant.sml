signature SEMANT =
sig
  val transProg : Absyn.exp -> {exp:Translate.frag list, ty:Types.ty}
end 

structure Semant :> SEMANT = struct
  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure T = Types
  structure Tr = Translate
  
  val err = ErrorMsg.error
  exception ErrMsg

  val nestLevel = ref 0
  
  fun incLevel () = nestLevel := !nestLevel + 1
  fun decLevel () = nestLevel := !nestLevel - 1
  fun typelookup tenv n pos= 
    (case S.look (tenv, n) of
        SOME ty2 => ty2
      | NONE => (err pos ("type is not defined: " ^ S.name n) ;
                T.UNIT))
  
  fun checkDups([],p) = ()
    | checkDups(h,[]) = ()
    | checkDups(h::l,p::ps) = 
        if (List.exists (fn (x) => (h = x)) l) then
          (err p (" duplicate entry: " ^ (S.name h)))
        else checkDups(l,ps)
        
  fun transTy (tenv, t)=
    let 
      fun recordtys(fields)= 
        map (fn{name, escape, typ, pos}=>
          (case SOME(typelookup tenv typ pos) of 
             SOME t => (name, t)
           | NONE => (name, T.UNIT))) fields
    in
      case t of
        A.NameTy (n, pos) => typelookup tenv n pos
      | A.RecordTy fields => (checkDups((map #name fields), (map #pos fields)); 
                              T.RECORD (recordtys fields, ref()))
      | A.ArrayTy (n,pos) => T.ARRAY(typelookup tenv n pos, ref())
    end
  
  fun compare_ty (ty1, ty2, pos)=
    case (ty1, ty2) of 
      (T.RECORD(l1,u1), T.RECORD(l2,u2)) => (u1=u2) andalso
        (List.all (fn ((s1,t1),(s2,t2)) => (s1=s2) andalso (t1=t2)) (ListPair.zip(l1,l2)))
    | (T.RECORD(_,_), T.NIL) => true
    | (T.NIL, T.RECORD(_,_)) => true
    | (_,_) => (ty1 = ty2)

  fun compare_tys ([], trexps, pos) = {exp=Tr.empty, ty=T.UNIT}
    | compare_tys(tys, [], pos) = {exp=Tr.empty, ty=T.UNIT}
    | compare_tys(t1::l1,t2::l2,pos) = (compare_ty(t1,t2,pos); compare_tys(l1,l2,pos))
  
  fun actual_ty (T.NAME (s,ty)) = 
      (case !ty of
        NONE => raise ErrMsg
      | SOME t => actual_ty t)
    | actual_ty t = t 

  fun checkInt ({exp, ty}, pos) =
    ((if ty = T.INT then () else err pos "integer required"); exp)

  fun checkUnit ({exp, ty}, pos) =
    ((if ty = T.UNIT then () else err pos "unit required"); exp)

  fun checkString ({exp, ty}, pos) =
    ((if ty = T.STRING then () else err pos "string required"); exp)
      
  fun checkRecord (rty, {exp, ty}, pos) =
    ((if compare_ty(rty, ty, pos) then () else err pos "record required"); exp)
    
  fun checkArray (rty, {exp, ty}, pos) =
    ((if compare_ty(rty, ty, pos) then () else err pos "array required"); exp)
    
   (* Takes venv, tenv, exp *)
  fun transExp(venv, tenv, break, level)  = 
    let 
      fun trexp (A.NilExp) = {exp=Tr.nilExp(), ty=T.NIL}
        | trexp (A.VarExp var) = trvar var
        | trexp (A.IntExp i) = {exp=(Tr.intExp(i)), ty=T.INT}
        | trexp (A.StringExp (str, pos)) = {exp=Tr.stringExp(str), ty=T.STRING}
        | trexp (A.OpExp {left, oper, right, pos}) = 
          let
            val left' = trexp left
            val right' = trexp right
          in
            if oper = A.PlusOp orelse oper = A.MinusOp orelse 
               oper = A.TimesOp orelse oper = A.DivideOp then
              (checkInt(left', pos); checkInt(right', pos);
               {exp=Tr.intOpExp(oper, #exp left', #exp right'), ty=T.INT})
            else if oper = A.LtOp orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.GeOp then
              (case #ty left' of
                T.INT =>
                  (checkInt(right', pos);
                  {exp=Tr.intOpExp(oper, #exp left', #exp right'), ty=T.INT})
              | T.STRING =>
                  (checkString(right', pos);
                  {exp=Tr.stringOpExp(oper, #exp left', #exp right'), ty=T.INT})
              | _ => (err pos "1can't perform comparisons on this type";
                    {exp=Tr.nilExp(), ty=T.INT}))
            else if oper = A.EqOp orelse oper = A.NeqOp then
              (case #ty left' of
                T.INT =>
                  (checkInt(right', pos);
                  {exp=Tr.intOpExp(oper, #exp left', #exp right'), ty=T.INT})
              | T.STRING =>
                  (checkString(right', pos);
                  {exp=Tr.stringOpExp(oper, #exp left', #exp right'), ty=T.INT})
              | T.RECORD(symtys, uq) => 
                  (checkRecord(T.RECORD(symtys, uq), right', pos);
                  {exp=Tr.recordCompExp(oper, #exp left', #exp right'), ty=T.INT})
              | T.NIL => 
                  (checkRecord(T.NIL, right', pos);
                  {exp=Tr.recordCompExp(oper, #exp left', #exp right'), ty=T.INT})
              | ty => (print ("-------"^T.toString(ty));
                      err pos "2can't perform comparisons on this type";
                      {exp=Tr.nilExp(), ty=T.INT}))
            else
              (err pos "error"; {exp=Tr.nilExp(), ty=T.INT})
        end
      | trexp (A.ArrayExp {typ, size, init, pos}) =
          let
            val aty = typelookup tenv typ pos
            val s = trexp size
            val i = trexp init
          in
            (checkInt(s, pos);
            (case aty of
              T.ARRAY(ty,_) => (checkArray(actual_ty ty, i, pos);())
            | _ => (err pos (" not an array type")));
            {exp=Tr.arrayExp(#exp s, #exp i), ty=aty})
          end

      | trexp (A.CallExp {func, args, pos}) = 
          (case S.look (venv, func) of
            SOME (E.FunEntry {formals, result, level, label}) =>
              let val args' = map trexp args
              in
                (if length(args) <> length(formals) then (* check argument lengths and compare *) 
                  (err pos "wrong amount of arguments"; {exp=Tr.nilExp(), ty=result})
                else
                  (compare_tys (formals, map #ty args', pos);
                  {exp=Tr.callExp(level,label,map #exp (map trexp args)),ty=actual_ty result}))
              end
            | _ => (err pos ("can't call nonexistent function: " ^ S.name func ); {exp=Tr.nilExp(), ty=T.UNIT}))
            (* Should check to make sure return types match, as do argtypes *)

      | trexp   (A.IfExp {test, then', else', pos}) =
          let
            val test' = trexp (test)
            val then'' = trexp (then')
          in
            (case else' of
              NONE =>
                (checkInt (test', pos); checkUnit(then'', pos);
                {exp=(Tr.ifThenExp(#exp test', #exp then'')), ty=(#ty then'')})
            | SOME else' =>
                let val else'' = trexp (else')
                in
                  checkInt(test', pos);
                  if compare_ty(#ty then'', #ty else'', pos) then ()
                  else (err pos "then and else clauses don't match in type"; ());
                  {exp=(Tr.ifThenElseExp(#exp test', #exp then'', #exp else'')), ty=T.UNIT}
                end
            )
          end

      | trexp (A.WhileExp {test, body, pos}) =
          let
            val _ = incLevel()
            val body' = transExp (venv,tenv,break,level) body
            val test' = transExp (venv,tenv,break,level) body
            val _ = decLevel()
          in
            (checkInt (test', pos); checkUnit (body', pos);
            {exp=Tr.whileExp(#exp test', #exp body', break), ty=T.UNIT})
          end 

      | trexp (A.RecordExp {fields, typ, pos}) =
          let 
            val typ' = typelookup tenv typ pos
            val result = actual_ty typ'
            val fnames = map #1 fields
            val tyfields = map trexp (map #2 fields)
            val types = map #ty tyfields
          in 
            case result of
              T.RECORD(s, u) =>
              let 
                val dfnames = map #1 s
                val dftypes = map actual_ty (map #2 s)
              in
                if fnames = dfnames then
                  if (ListPair.all
                      (fn (ty1, ty2) => compare_ty (ty1, ty2, pos))
                      (types, dftypes)) then
                      {exp=Tr.recordExp(map #exp tyfields), ty=result} 
                  else 
                    (err pos ("field types not consistent: " ^ S.name typ);
                    {exp=Tr.nilExp(),ty=result})
                else
                  (err pos ("field types not consistent: " ^ S.name typ);
                  {exp=Tr.nilExp(),ty=result})
              end
            | _ => (err pos ("not a valid record type: " ^ S.name typ);
              {exp=Tr.nilExp(), ty=T.UNIT})
          end
        
      | trexp (A.SeqExp []) = {exp=Tr.empty, ty=T.UNIT}
      | trexp (A.SeqExp exps) =
          let
            val (exps', ty) =
              foldl (fn ((exp, _), (exps', _)) =>
                      let val {exp=newExp, ty} = 
                            (transExp(venv, tenv, break, level) exp)
                      in (exps' @ [newExp], ty) end)
              ([], T.UNIT)
              exps
          in
             {exp=Tr.seqExp exps', ty=ty}
           end
         
      | trexp (A.AssignExp {var, exp, pos}) =
          let
            val  {exp=left,  ty=expect} = trvar (var)
            val  {exp=right, ty=actual} = trexp (exp)
          in
            if expect <> actual then
              (err pos "assignment mismatch"; {exp=Tr.nilExp(), ty=T.UNIT})
            else
              {exp=Tr.assignExp(left, right), ty=T.UNIT}
          end

      | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
        let
          val access = Tr.allocLocal level (!escape)
          val breakpoint = Tr.newbreakpoint()
          val lo' = trexp lo
          val hi' = trexp hi
          val venv' = S.enter (venv, var, Env.VarEntry {access=access, ty=T.INT})
          val {exp=bodyExp, ty=body_ty} = transExp (venv', tenv, breakpoint, level) body
        in
          (checkInt(lo', pos); checkInt(hi', pos);
          {exp=Tr.forExp(Tr.simpleVar (access, level), breakpoint, #exp lo', #exp hi', bodyExp), ty=T.UNIT})
        end

      | trexp (A.BreakExp pos) =
          if !nestLevel > 0 then
            {exp=Tr.breakExp(break), ty=T.UNIT}
          else
            (err pos "Break not nested correctly";
              {exp=Tr.nilExp(), ty=T.UNIT})

      | trexp (A.LetExp {decs, body, pos}) =
          let
            val (venv', tenv', decList, _) =
              foldr (fn (dec, (v, t, e, l)) => transDec(v, t, dec, break, e, l))
                (venv, tenv, [], level) decs
            val {exp=bodyExp, ty=bodyTy} = transExp (venv',tenv', break, level) body
          in
            {exp=Tr.letExp(decList,bodyExp), ty=bodyTy}
          end

     and trvar (A.SimpleVar(id,pos)) = 
          (case S.look(venv, id) of
            SOME (E.VarEntry{access, ty}) => 
              {exp=Tr.simpleVar(access, level), ty=actual_ty ty}
          | _ => (err pos ("undefined variable: " ^ S.name id); 
              {exp=Tr.nilExp(), ty=T.INT}))

      | trvar (A.FieldVar(var,id,pos)) =
          (case trvar var of
            {exp, ty=record as T.RECORD (fields, _)} => {exp=Tr.nilExp(), ty=record}
          | _ => (err pos "no var found"; {exp=Tr.nilExp(), ty=T.UNIT}))

      | trvar (A.SubscriptVar(var, exp, pos)) =
          let
            val {exp=v,ty} = trvar var
            val subs = trexp exp
          in
            (checkInt(subs, pos);
            (case ty of 
              T.ARRAY(typ, _) => 
                    {exp=Tr.subscriptExp(v, #exp subs), ty=actual_ty typ}
            | _ => (err pos ("not an array type");
                    {exp=Tr.nilExp(), ty=T.UNIT})))
          end
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
                      (case S.look (tenv,s) of
                          NONE => (err pos ("type not defined: " ^ S.name s))
                        | SOME ty2 => (compare_ty(ty, ty2, pos1);()))
                  | NONE => ()
        in
          (S.enter(venv, name, E.VarEntry{access=access, ty=ty}), tenv, explist', level)
        end

    | transDec (venv, tenv, A.TypeDec tydecs, break, explist, level) = 
        let
          val names = map #name tydecs
          val poss = map #pos tydecs
          val _ = checkDups(names, poss)
          val typs = map #ty tydecs
          fun addt (n,env) = S.enter(env,n,T.NAME(n,ref(S.look(tenv, n))))
          val tenv' = foldr addt tenv names
          val nts = map (fn t => transTy (tenv', t)) typs
          fun updt (n,nt) = 
            let val (SOME (T.NAME(_,r))) = S.look(tenv',n)
            in r := SOME nt
            end
          val _ = app updt (ListPair.zip(names,nts))
        in 
          (venv, tenv', explist, level)
        end

    | transDec(venv, tenv, A.FunctionDec(fundecs), break, explist, level) =
        let
          val fundef = ref []
          val levels = ref []

          fun makeheader (fundec:A.fundec,env) = 
            let
              val r_ty = 
                (case (#result fundec) of
                  NONE => T.UNIT
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
              S.enter(env,
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
              S.enter(venv, (#name param), E.VarEntry{access=access,ty=ty})
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
              fundef := (!fundef) @ [exp]
            end

          val checkres = (map check fundecs)
          val fundef' = !fundef
          val explist' = (explist@fundef')
        in
          (venv', tenv, explist', level)
        end
    
    fun transProg(exp) =
      let
        val _ = Tr.init()
        val firstlevel = Translate.newLevel {
          name=Temp.namedlabel "firstlevel",
          parent=Tr.outermost,
          formals=[]}

        val {exp, ty} = transExp (Env.base_venv, Env.base_tenv, Temp.newlabel(), firstlevel) exp
      in
        (Tr.procEntryExit {level=firstlevel, body=exp};
        {exp=Tr.getResult(), ty=ty})
      end
end