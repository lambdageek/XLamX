module XLamX.Examples

open Expr
let t1 = ArrT (BaseT, BaseT)
let e1 = Lam ("x", BaseT, Var "x")
let e2 = Lam ("f", ArrT (BaseT, BaseT), Lam ("y", BaseT, App (Var "f", Var "y")))
let e3 = App (e2, e1)
let e4 = TLam (mkTyvar "a", TypeK, Lam ("x", VarT (mkTyvar "a"), Var "x"))
let e5 = App (e2, TApp (e4, BaseT))