// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open XLamX.Evaluation

let rec evalSteps m =
    match Machine.final m with
      | Some v -> printfn "Result: %A" v
      | None ->
        let m' = Machine.step m 
        printfn "%A \u2907 %A" m m'
        evalSteps m'

[<EntryPoint>]
let main argv = 
    evalSteps (Machine.initial Value.emptyEnv XLamX.Examples.e3)
    0 // return an integer exit code

