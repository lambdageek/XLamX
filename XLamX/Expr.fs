module XLamX.Expr

  type Kind =
      | TypeK

  type tyvar = (string * int)

  type Type =
      | BaseT
      | VarT of tyvar
      | ArrT of (Type * Type)
      | ForallT of (tyvar * Kind * Type)
  
  type var = string

  type Expr =
       | Var of var
       | Lam of (var * Type * Expr)
       | App of Expr * Expr
       | TLam of (tyvar * Kind * Expr)
       | TApp of (Expr * Type)

  let mkTyvar s = (s, 0) (* stale! *)