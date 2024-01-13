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

    let vm = Lexer.StackMachine()
    try
        vm.PUSH 1
        vm.PUSH 3
        vm.ADD()
        // 4
        vm.PUSH 0
        vm.DIV()
        vm.Print()
    with
    | Lexer.Err(e) -> printfn $"{e}"

    0
