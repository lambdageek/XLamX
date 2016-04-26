namespace XLamX

open Xamarin.Forms

module Stepper =
   open XLamX.Evaluation

   type State =
     | Finished of Value.Value
     | Stepping of Machine.Machine
     | Empty
   
   let (state : State ref) = ref Empty 

   let displayState s (a : Label) =
       match s with
         | Empty -> a.Text <- "(Not initialized)"
         | Stepping m -> a.Text <- sprintf "{Machine %A}" m
         | Finished v -> a.Text <- sprintf "{Value %A}" v

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

   let initialize (a : Label) =
        state := Stepping (Machine.initial Value.emptyEnv XLamX.Examples.e3)
        displayState (!state) a


type App() = 
    inherit Application()
    let stack = StackLayout(VerticalOptions = LayoutOptions.Center, Spacing = 20.0)
    let label = Label(HorizontalTextAlignment = TextAlignment.Center, Text = "XLamX")
    let evalArea = Label(HorizontalTextAlignment = TextAlignment.Start);
    let button = Button(Text = "|=>")
    do 
        stack.Children.Add(label)
        stack.Children.Add(evalArea)
        stack.Children.Add(button)
        Stepper.initialize (evalArea)
        button.Clicked.AddHandler (System.EventHandler (fun sender args -> Stepper.clickyclicky evalArea))
        base.MainPage <- ContentPage(Content = stack)

