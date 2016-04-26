namespace XLamX.Typechecking

module StaticsErrors =
    open XLamX.Expr

    type TypecheckingError =
      | UnboundVariable of string
      | ExpectedFunctionType of Type
      | ExpectedMatchingTypes of (Type * Type)
      | Unimplemented of string

    exception TypeError of TypecheckingError

    let typeError e = raise (TypeError e)

module TC =
    open XLamX.Expr
    open StaticsErrors

    type Env = (var * Type) list

    let (emptyEnv : Env) = []

    let extendEnv (env : Env) x t = (x,t)::env

    let rec lookupEnv (env : Env) x =
        match env with
          | ((y,t)::_) when x = y -> Some t
          | (_::env') -> lookupEnv env' x
          | [] -> None

    let rec ensureWfType t = () (* all types WF in STLC *)

    let rec ensureEquivTypes t1 t2 =
        if t1 = t2 then ()
        else typeError (ExpectedMatchingTypes (t1, t2))

    let ensureFnType t =
        match t with
          | ArrT ts -> ts
          | _ -> typeError (ExpectedFunctionType t)

    let rec infer env e =
     match e with
       | Var x ->
           match lookupEnv env x with
             | Some t -> t
             | None -> typeError (UnboundVariable x)
       | Lam (x, tdom, e') ->
           ensureWfType tdom
           let tcod = infer (extendEnv env x tdom) e'
           ArrT (tdom, tcod)
       | App (e1, e2) ->
           let t1 = infer env e1
           let (tdom, tcod) = ensureFnType t1
           check env e2 tdom
           tcod

    and check env e texpect =
        let tactual = infer env e
        ensureEquivTypes tactual texpect
            