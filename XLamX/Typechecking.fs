namespace XLamX.Typechecking

module MonadReaderExcept =
    type M<'e, 'r, 'a> = 'r -> Choice<'e, 'a>

    let bind c f = fun env ->
        match c env with
          | Choice1Of2 err -> Choice1Of2 err
          | Choice2Of2 x -> f x env

    let ret x = fun env -> Choice2Of2 x

    let combine comp1 comp2 = fun env ->
        match comp1 env with
          | Choice1Of2 err -> Choice1Of2 err
          | Choice2Of2 () -> comp2 env
    let ask () = fun env -> Choice2Of2 env

    let local modifyFn thunk =
        fun env -> thunk () (modifyFn env)

    let throwError (err : 'e) : M<'e, 'r, 'a> = fun env -> Choice1Of2 err
    let catchError thunk handler = fun env ->
        match thunk () env with
          | Choice1Of2 err -> handler err env
          | Choice2Of2 v -> Choice2Of2 v

    let runReaderExcept (comp : M<'e,'r,'a>) (env : 'r) : Choice<'e, 'a> = comp env

type MonadReaderExceptBuilder<'e,'r>() =
    member x.Bind(comp,func) = MonadReaderExcept.bind comp func
    member x.Return(value) = MonadReaderExcept.ret value
    member x.ReturnFrom(comp) = comp

module StaticsErrors =
    open XLamX.Expr

    type TypecheckingError =
      | UnboundVariable of var
      | UnboundTypeVariable of tyvar
      | ExpectedFunctionType of Type
      | ExpectedForallType of Type
        (* the two types that didn't match and an underlying reason why, if available *)
      | ExpectedMatchingTypes of (Type * Type * TypecheckingError option)
      | Unimplemented of string

    exception TypeError of TypecheckingError

    let typeError e = raise (TypeError e)

module TC =
    open XLamX.Expr
    open StaticsErrors
    open MonadReaderExcept

    type Env = (tyvar * Kind) list * (var * Type) list

    let tc = MonadReaderExceptBuilder<TypecheckingError, Env>()

    type TypeChecker<'a> = Env -> Choice<TypecheckingError,'a>

    let emptyEnv : Env = ([], [])

    let lookupEnv x : TypeChecker<Type> = 
        let rec go vs =
          match vs with
            | ((y,t)::_) when x = y -> Some t
            | (_::vs') -> go vs'
            | [] -> None
        tc { let! (tenv, venv) = ask ()
             match go venv with
               | Some t -> return t
               | None -> return! throwError (UnboundVariable x)
           }

    let lookupEnvTy a : TypeChecker<Kind> =
        let rec go vs =
            match vs with
              | ((b,k)::_) when a = b -> Some k
              | (_::vs') -> go vs'
              | [] -> None

        tc { let! (tenv, venv) = ask ()
             match go tenv with
               | Some k -> return k
               | None -> return! throwError (UnboundTypeVariable a)
           }

    let extendEnv x t thunk = 
        let modify (tenv,venv) = (tenv, (x,t)::venv)
        local modify thunk
    let extendEnvTy a k thunk =
        let modify (tenv,venv) = ((a,k)::tenv, venv)
        local modify thunk

    let rec inferKind (t : Type) : TypeChecker<Kind> =
        tc { match t with
               | BaseT -> return TypeK
               | VarT a -> return! lookupEnvTy a
               | ArrT (t1, t2) -> do! checkTy t1 TypeK
                                  do! checkTy t2 TypeK
                                  return TypeK
               | ForallT (a, k, t') ->
                   do! extendEnvTy a k (fun () -> checkTy t' TypeK)
                   return TypeK
           }

    and checkTy t k =
        tc { let! kout = inferKind t
             match (k, kout) with
               | (TypeK, TypeK) -> return ()
           }


    (* Will throwError when the types don't match.
       Not super-usable as is, because it will be down in the weeds. *)
    let rec eqTy t1 t2 : TypeChecker<Kind> =
        tc { match (t1, t2) with
               | (BaseT, BaseT) -> return TypeK
               | (VarT a, VarT b) when a = b -> return! lookupEnvTy a
               | (ArrT (t1, t2), ArrT (s1, s2)) ->
                   do! eqTy' t1 s1 TypeK
                   do! eqTy' t2 s2 TypeK
                   return TypeK
               | (ForallT (a, k, t), ForallT (b, k', s)) when k = k' ->
                   let a' = XLamX.TypeSubst.fresh a
                   let t' = XLamX.TypeSubst.renameFreshTyTy a' a t
                   let s' = XLamX.TypeSubst.renameFreshTyTy a' b s
                   do! extendEnvTy a' k (fun () -> eqTy' t' s' TypeK)
                   return TypeK
               | _ -> return! throwError (ExpectedMatchingTypes (t1, t2, None))
          }
    and eqTy' t1 t2 k : TypeChecker<unit> =
        tc { let! kout = eqTy t1 t2
             match (k, kout) with
               | (TypeK, TypeK) -> return ()
           }
    (* this is the nicer eq checker which tells you the outermost types being compared,
       and then place where things went bad *)
    let ensureEquivTypes t1 t2 : TypeChecker<unit> =
        catchError (fun () -> eqTy' t1 t2 TypeK)
                   (fun err -> throwError (ExpectedMatchingTypes (t1, t2, Some err)))

    let ensureFnType (t : Type) : TypeChecker< Type * Type > =
        tc { match t with
               | ArrT ts -> return ts
               | _ -> return! throwError (ExpectedFunctionType t)
           }
            
    let ensureAllType t : TypeChecker< tyvar * Kind * Type > =
        tc { match t with
               | ForallT akt -> return akt
               | _ -> return! throwError (ExpectedForallType t)
           }
    let rec infer e : TypeChecker<Type> =
        tc { match e with
               | Var x -> return! lookupEnv x
               | Lam (x, tdom, e') ->
                   do! checkTy tdom TypeK
                   let! tcod = extendEnv x tdom (fun () -> infer e')
                   return ArrT (tdom, tcod)
               | App (e1, e2) ->
                    let! t1 = infer e1
                    let! (tdom, tcod) = ensureFnType t1
                    do! check e2 tdom
                    return tcod
               | TLam (a, k, e) ->
                    let! t = extendEnvTy a k (fun () -> infer e)
                    return ForallT (a, k, t)
               | TApp (e, targ) ->
                    let! t = infer e
                    let! (a, k, tcod) = ensureAllType t
                    do! checkTy targ k
                    return XLamX.TypeSubst.tyTySubst targ a tcod
           }

    and check e texpect : TypeChecker<unit> =
        tc { let! tactual = infer e
             do! ensureEquivTypes tactual texpect
           }
    let runTC (env : Env) (comp : TypeChecker<'a>) : Choice<TypecheckingError, 'a> =
      runReaderExcept comp env

    let runTCIO env comp =
      match runTC comp env with
        | Choice1Of2 err -> raise (TypeError err)
        | Choice2Of2 v -> v


    
