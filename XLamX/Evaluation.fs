namespace XLamX.Evaluation

  module RuntimeErrors =
    type EvalErrorMsg =
        | UnboundVariable of string
        | Unimplemented of string

    exception EvalError of EvalErrorMsg

    let evalError e = raise (EvalError e)


  module Value =
    open XLamX.Expr
    type Value =
        | ClosureV of Closure

    and Closure = (Env * (var * Type * Expr))

    and Env = (var * Value) list

    let extendEnv env x v = (x,v) :: env

    let emptyEnv = []

    let rec lookupEnv env x =
        match env with
          | ((y,v)::env') when x = y -> Some v
          | (_::env') -> lookupEnv env' x
          | [] -> None 

  module Eval =
    open XLamX.Expr
    open RuntimeErrors
    open Value

    let rec eval (env : Env) (e : Expr) : Value =
        match e with
          | Lam xte ->
              ClosureV (env, xte)
          | App (e1, e2) ->
              let v1 = eval env e1
              let v2 = eval env e2
              match v1 with
                ClosureV (env', (x, _t, ebody)) ->
                    eval (extendEnv env' x v2) ebody
          | Var x ->
              match lookupEnv env x with
                | Some v -> v
                | None -> evalError (UnboundVariable x)
          (*          | _ -> evalError (Unimplemented "like... everything") *)

  module Machine =
    open XLamX.Expr
    open RuntimeErrors
    open Value

    type Cont = 
        | FrameK of Frame * Cont
        | AwaitK of Env * Cont
        | HaltK

    and Frame =
        | AppF1 of Expr
        | AppF2 of Value
    

    type Cmd = 
        RetC of Value * Cont
        | PushC of Expr * Cont

    type Machine = Env * Cmd

    let initial (env : Env) (e : Expr) = (env, PushC (e, HaltK) )

    let final (m : Machine) : Value option = 
       match m with 
        | (env, RetC (v, HaltK)) -> Some v
        | _ -> None

    let rec step (m : Machine) : Machine =
        match m with
          | (env, PushC (Lam xte, k)) -> (env, RetC (ClosureV (env, xte), k))
          | (env, PushC (App (e1, e2), k)) -> (env, PushC (e1, FrameK (AppF1 e2, k)))
          | (env, PushC (Var x, k)) ->
              match lookupEnv env x with
                | Some v -> (env, RetC (v, k))
                | None -> evalError (UnboundVariable x)
          | (env, (RetC (_, HaltK) as fin)) -> (env, fin) (* just loop *)
          | (env, RetC (v, FrameK (f, k))) ->
              match f with
                | AppF1 e2 -> (env, PushC (e2, FrameK (AppF2 v, k)))
                | AppF2 vlam ->
                    match vlam with
                      | ClosureV (env', (x, _t, ebody)) -> (extendEnv env' x v, PushC (ebody, AwaitK (env, k)))
          | (env, RetC (v, AwaitK (env', k))) -> (env', RetC (v, k))
