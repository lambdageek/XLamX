namespace XLamX.Typechecking

module StaticsErrors =
    open XLamX.Expr

    type TypecheckingError =
      | UnboundVariable of string
      | ExpectedFunctionType of Type
      | ExpectedForallType of Type
      | ExpectedMatchingTypes of (Type * Type)
      | Unimplemented of string

    exception TypeError of TypecheckingError

    let typeError e = raise (TypeError e)

module TC =
    open XLamX.Expr
    open StaticsErrors

    type Env = (tyvar * Kind) list * (var * Type) list 

    let (emptyEnv : Env) = ([], [])

    let extendEnv ((tenv, venv) : Env) x t = (tenv, (x,t)::venv)
    let extendEnvTy ((tenv, venv) : Env) a k = ((a,k)::tenv, venv)

    let lookupEnv ((tenv,venv) : Env) x =
        let rec go vs = 
          match vs with
            | ((y,t)::_) when x = y -> Some t
            | (_::vs') -> go vs'
            | [] -> None
        go venv
    
    let lookupEnvTy ((tenv, venv) : Env) a =
        let rec go vs =
            match vs with
              | ((b,k)::_) when a = b -> Some k
              | (_::vs') -> go vs'
              | [] -> None
        go tenv

    let rec ensureWfType env t =
        match t with
          | BaseT -> ()
          | VarT a ->
              let TypeK = lookupEnvTy env a
              ()
          | ArrT (t1, t2) ->
              ensureWfType env t1
              ensureWfType env t2
          | ForallT (a, k, t) ->
              ensureWfType (extendEnvTy env a k) t

    let checkTy env t TypeK = ensureWfType env t

    let rec eqTy env t1 t2 =
        match (t1, t2) with
          | (BaseT, BaseT) -> true
          | (VarT a, VarT b) -> a = b
          | (ArrT (t1, t2), ArrT (s1, s2)) ->
              eqTy env t1 s1 && eqTy env t2 s2
          | (ForallT (a, k, t), ForallT (b, k', s)) when k = k' ->
              let a' = XLamX.TypeSubst.fresh a
              let t' = XLamX.TypeSubst.renameFreshTyTy a' a t
              let s' = XLamX.TypeSubst.renameFreshTyTy a' b s
              let env' = extendEnvTy env a' k
              eqTy env' t' s'
          | _ -> false

    let rec ensureEquivTypes env t1 t2 =
        if eqTy env t1 t2 then ()
        else typeError (ExpectedMatchingTypes (t1, t2))

    let ensureFnType t =
        match t with
          | ArrT ts -> ts
          | _ -> typeError (ExpectedFunctionType t)
    
    let ensureAllType t =
        match t with
          | ForallT akt -> akt
          | _ -> typeError (ExpectedForallType t)

    let rec infer env e =
     match e with
       | Var x ->
           match lookupEnv env x with
             | Some t -> t
             | None -> typeError (UnboundVariable x)
       | Lam (x, tdom, e') ->
           ensureWfType env tdom
           let tcod = infer (extendEnv env x tdom) e'
           ArrT (tdom, tcod)
       | App (e1, e2) ->
           let t1 = infer env e1
           let (tdom, tcod) = ensureFnType t1
           check env e2 tdom
           tcod
       | TLam (a, k, e) ->
           let t = infer (extendEnvTy env a k) e
           ForallT (a, k, t)
       | TApp (e, targ) ->
           let t = infer env e
           let (a, k, tcod) = ensureAllType t
           checkTy env targ k
           XLamX.TypeSubst.tyTySubst targ a tcod


    and check env e texpect =
        let tactual = infer env e
        ensureEquivTypes env tactual texpect
            