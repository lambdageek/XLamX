# XLamX #

## A λ-Calculus stepper for iOS and Android ##

This is a Xamarin.Forms project that builds a simple stepper for lambda calculus expressions.

All the non-boilerplate bits are in `XLamX/`.  The main logic of the stepper is in `XLamX/Evaluation.fs` in the `Stepper` module (it's basically a CEK machine with an explicit control - "I'm breaking down an expression to find stuff to evaluate"/"I am done evaluating and am returning the value to a frame that will consume it" - and a frame that marks the end of a function body execution (so the control stack grows even on tail calls, though it's easy to check for that by peeking)).

The "GUI" part is in `XLamX/XLamX.fs`.  There's no XAML so it's all built in code.  Right now you don't get to pick which expression we evaluate.  You get to click "|=>" to see the steps until it's done and then you quit the app.  "Features" coming later, provided my attention doesn't wander off.

### "Tasting notes" from those responsible ###

Bear in mind this is:
* about the 50th language interpreter,
* 10th or so graphical UI,
* 2nd F# program,
* 1st Xamarin.Forms app,
* 1st mobile app of any sort

that I've written.  So I'm probably doing a lot of dumb things.  I am still figuring out good F# habits, and I'm sure the other visual bits could be done way better.  So don't go copying the ugly parts.
