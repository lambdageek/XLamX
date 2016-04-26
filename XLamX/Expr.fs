module XLamX.Expr

  type Type =
      | BaseT
      | ArrT of Type * Type
  
  type var = string

  type Expr =
       | Var of var
       | Lam of (var * Type * Expr)
       | App of Expr * Expr  
