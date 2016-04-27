namespace XLamX

open Xamarin.Forms

module Stepper =
   open XLamX.Evaluation

   type State =
     | Finished of Value.Value
     | Stepping of Machine.Machine
     | TypeError
     | Empty
   
   let (state : State ref) = ref Empty 

   let displayState s (a : Label) =
       a.Text <- match s with
                   | Empty -> "(Not initialized)"
                   | TypeError -> "Unsafe to run code that doesn't type check!" 
                   | Stepping m -> sprintf "Machine State:\n%A" m
                   | Finished v -> sprintf "Machine Finished with Answer:\n%A" v

   let updateState s =
       match s with
         | Stepping m -> 
             match Machine.final m with
               | Some v -> Finished v
               | None -> Stepping (Machine.step m)
         | boring -> boring

   let clickyclicky (a : Label) =
       state := updateState (!state)
       displayState (!state) a

   let initialize (expr : XLamX.Expr.Expr, ev : Label, ty : Label) =
       try
           let t = Typechecking.TC.infer Typechecking.TC.emptyEnv expr
           let m = Machine.initial Value.emptyEnv expr
           state := Stepping m
           displayState (!state) ev
           ty.Text <- sprintf "Type: %A" t
       with
         | XLamX.Typechecking.StaticsErrors.TypeError err ->
             state := TypeError
             displayState (!state) ev
             ty.Text <- sprintf "Type Error: %A" err

   type example = string * XLamX.Expr.Expr

   let handy (navvy : NavigationPage) f _ (args : SelectedItemChangedEventArgs) = 
     let (name, ex) = downcast args.SelectedItem
     ignore (navvy.PushAsync (f name ex))

type EvalPage(name : string, expr : XLamX.Expr.Expr) =
    inherit ContentPage()
    let stack = StackLayout(Spacing = 20.0)
    let resetButton = Button (Text = "Start Again", VerticalOptions = LayoutOptions.Start)
    let typeArea = Label(HorizontalTextAlignment = TextAlignment.Start, VerticalOptions = LayoutOptions.Center)
    let evalArea = Label(HorizontalTextAlignment = TextAlignment.Start, VerticalOptions = LayoutOptions.CenterAndExpand)
    let steppybutton = Button(Text = "|=>", VerticalOptions = LayoutOptions.End)
    do 
        stack.Children.Add(resetButton)
        stack.Children.Add(typeArea)
        stack.Children.Add(evalArea)
        stack.Children.Add(steppybutton)
        Stepper.initialize (expr, evalArea, typeArea)
        resetButton.Clicked.AddHandler (System.EventHandler (fun _ _ -> Stepper.initialize (expr, evalArea, typeArea)))
        steppybutton.Clicked.AddHandler (System.EventHandler (fun sender args -> Stepper.clickyclicky evalArea))
        base.Content <- stack
        base.Title <- sprintf "XLamX: %s" name

type ExamplesPage (navvy : NavigationPage) =
    inherit ContentPage()
    let listy = ListView()
    do
        listy.ItemsSource <- [("e1", XLamX.Examples.e1);
                              ("polyId", XLamX.Examples.polyId);
                              ("e5", XLamX.Examples.e5);
                              ("e6bad", XLamX.Examples.e6bad)
                             ]
        listy.ItemSelected.AddHandler (System.EventHandler<_> (Stepper.handy navvy (fun name expr -> new EvalPage (name, expr))))
        base.Content <- listy
        base.Title <- "Select an example"

type App() = 
    inherit Application()
    let navvy = new NavigationPage()
    let listy = new ExamplesPage (navvy) (*new EvalPage ("e5", XLamX.Examples.e5(*e6bad*))*)
    do
        ignore (navvy.PushAsync (listy, false))
        base.MainPage <- navvy

