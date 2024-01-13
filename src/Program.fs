open System
open Lexer

let getData path =
    (String.concat "\n" (IO.File.ReadLines(path) |> Seq.toList))
    

[<EntryPoint>]
let main _ =
    let expression = getData "./example.lisp"

    printfn $"{expression}"

    try
        let vm = StackMachine.StackMachine()
        Parser.checkForCorrectBrackets expression
        Parser.parse expression vm
    with
    | Err(e) -> 
        printfn $"{e}"

    0