open System
open Lexer

let getData path =
    (String.concat "\n" (IO.File.ReadLines(path) |> Seq.toList))
    

[<EntryPoint>]
let main _ =
    let expression = getData "./example"

    printfn $"{expression}"

    try
        Parser.checkForCorrectBrackets expression
        let res = Parser.parse expression
        printfn $"Result: {res}"
    with
    | Err(e) -> 
        printfn $"{e}"

    0