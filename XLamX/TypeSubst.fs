module XLamX.TypeSubst

open XLamX.Expr

let fresh =
    let s = ref 0
    fun (x : tyvar) ->
        let x' = (fst x, !s)
        s := (!s) + 1
        x'

let renameFreshTyTy (fresh : tyvar) (stale : tyvar) =
    let rec go t =  
      match t with
        | BaseT -> t
        | VarT b when stale = b -> VarT fresh
        | VarT _ -> t
        | ArrT (tdom, tcod) -> ArrT (go tdom, go tcod)
        | ForallT (b, k, t') when stale = b -> t (* shadowing *)
        | ForallT (b, k, t') -> ForallT (b, k, go t')
    go

let renameFreshTy (fresh : tyvar) (stale : tyvar) =
    let rec go (e : Expr) =
        match e with
            | Var _ -> e
            | Lam (x, t, e) -> Lam (x, renameFreshTyTy fresh stale t, go e)
            | App (e1, e2) -> App (go e1, go e2)
            | TLam (b, k, e') when stale = b -> e (* shadowing *)
            | TLam (b, k, e') -> TLam (b, k, go e')
            | TApp (e, t) ->  TApp (go e, renameFreshTyTy fresh stale t)
    go

let tyTySubst (s : Type) (a : tyvar) =
    let rec go (t : Type) = 
      match t with
        | BaseT -> t
        | VarT b when a = b -> s
        | VarT _ -> t
        | ArrT (tdom, tcod) -> ArrT (go tdom, go tcod)
        | ForallT (b, k, t') when a = b -> t (* shadowing *)
        | ForallT (b, k, t') ->
            let b' = fresh b
            let t'' = renameFreshTyTy b' b t'
            ForallT (b', k, go t'')
    go

let rec tySubst (s : Type) (a : tyvar) =
    let rec go (e : Expr) = 
       match e with
       | Var _ -> e
       | Lam (x, t, e) -> Lam (x, tyTySubst s a t, go e)
       | App (e1, e2) -> App (go e1, go e2)
       | TLam (b, _, _) when a = b -> e (* shadowed *)
       | TLam (b, k, e') ->
           let b' = fresh b
           let e'' = renameFreshTy b' b e'
           TLam (b', k, go e'')
       | TApp (e, t) ->
           TApp (go e, tyTySubst s a t)
    go
