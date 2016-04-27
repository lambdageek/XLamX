module XLamX.Examples

open Expr
let t1 = ArrT (BaseT, BaseT)
let e1 = Lam ("x", BaseT, Var "x")
let polyId = TLam (mkTyvar "a", TypeK, Lam ("x", VarT (mkTyvar "a"), Var "x"))
let e2 = Lam ("f", ArrT (BaseT, BaseT), App (TApp (polyId, t1), Lam ("y", BaseT, App (Var "f", Var "y"))))
let e3 = App (e2, e1)
let e5 = App (e2, TApp (polyId, BaseT))
let e6bad = App (e2, TApp (polyId, ArrT (BaseT, BaseT)))