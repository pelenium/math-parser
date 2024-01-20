open System
open Lexer

let getData path = IO.File.ReadLines(path) |> Seq.toList

let splitStr (s: string) = String.concat "\n" (s.Split "\n")

[<EntryPoint>]
let main _ =
    let expressions = getData "./example"

    for e in expressions do
        printfn $"{e}"

    printfn ""

    try
        for e in expressions do
            e |> splitStr |> Parser.checkForCorrectBrackets
            let res = Parser.parse e
            printfn $"Result: {res}\n"
    with Err(e) ->
        printfn $"{e}"

    0
