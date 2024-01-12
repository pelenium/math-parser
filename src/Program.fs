open Lexer
open System

let getData path =
    (String.concat "\n" (IO.File.ReadLines(path) |> Seq.toList))
        .Replace("(", " ( ")
        .Replace(")", " ) ")
        .Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Trim())
    |> Array.toList

[<EntryPoint>]
let main _ =
    let code = getData "./example.lisp"

    for i in code do
        printf $"{i} "

    let tokens = Tokenizer.Tokenize code

    for i in tokens do
        printfn $"{i}"

    0
