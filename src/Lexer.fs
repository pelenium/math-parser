namespace Lexer

exception Err of string

type Token =
    | LPar
    | RPar
    | Number
    | Plus
    | Minus
    | Multiply
    | Divide

module StackMachine =

    type StackMachine() =
        let mutable Stack: int list = []

        member this.Len() = Stack.Length

        member _.PUSH(n: int) = Stack <- Stack @ [ n ]

        member _.POP() =
            let head = Stack[Stack.Length - 1]
            Stack <- Stack.GetSlice(Some 0, Some(Stack.Length - 2))

            head

        member this.ADD() =
            this.Print()

            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            this.PUSH(this.POP() + this.POP())

        member this.SUB() =
            this.Print()

            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            let fst = this.POP()
            let snd = this.POP()

            this.PUSH(snd - fst)

        member this.MUL() =
            this.Print()

            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            let fst = this.POP()
            let snd = this.POP()

            this.PUSH(fst * snd)

        member this.DIV() =
            this.Print()

            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            let snd = this.POP()
            let fst = this.POP()

            if snd = 0 then
                raise (Err "Error: Division by zero")

            this.PUSH(fst / snd) 

        member _.Print() =
            for i in Stack do
                printfn $"{i}"

            printfn ""

module private OperationStack =
    type OperationStack() =
        let mutable Stack: string list = []

        member _.PUSH(op: string) = Stack <- Stack @ [ op ]

        member _.POP() : string =
            let head = Stack[Stack.Length - 1]
            Stack <- Stack.GetSlice(Some 0, Some(Stack.Length - 2))

            head

module Tokenizer =
    type Token =
        | LPar
        | RPar
        | Number
        | Plus
        | Minus
        | Multiply
        | Divide
        | Identidier

    let internal ToInt (token: string) =
        let mutable result = 0

        match System.Int32.TryParse(token, &result) with
        | true -> Some(result)
        | _ -> None

    let (|INT|NONE|) (token: string) =
        match ToInt token with
        | Some _ -> INT
        | None -> NONE

    let Tokenize (code: string list) =
        List.map
            (fun l ->
                match l with
                | "(" -> (LPar, l)
                | ")" -> (RPar, l)
                | "+" -> (Plus, l)
                | "-" -> (Minus, l)
                | "*" -> (Multiply, l)
                | "/" -> (Divide, l)
                | INT _ -> (Number, l)
                | _ -> (Identidier, l))
            code

module Parser =
    open Tokenizer
    open System.Text.RegularExpressions

    let split (code: string) =
        code
            .Replace("(", " ( ")
            .Replace(")", " ) ")
            .Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
        |> Array.toList

    let ToInt (token: string) =
        let mutable result = 0

        let _ = System.Int32.TryParse(token, &result)
        result

    let parse (code: string) : int =
        let os = OperationStack.OperationStack()
        let sm = StackMachine.StackMachine()
        let tokens = code |> split |> Tokenize

        for token in tokens do
            // printfn $"{token}"

            match token with
            | (Plus, _) -> os.PUSH "+"
            | (Minus, _) -> os.PUSH "-"
            | (Multiply, _) -> os.PUSH "*"
            | (Divide, _) -> os.PUSH "/"
            | (Number, num) -> ToInt num |> sm.PUSH
            | (RPar, _) ->
                match os.POP() with
                | "+" -> sm.ADD()
                | "-" -> sm.SUB()
                | "*" -> sm.MUL()
                | "/" -> sm.DIV()
                | _ -> ()
            | _ -> ()

        let res = sm.POP()

        if sm.Len() = 0 then
            res
        else
            raise (Err "Error: Incorrect number sequence")

    let checkNum (code: string) =
        let a = Regex.Matches(code, Regex.Escape "(").Count
        let b = Regex.Matches(code, Regex.Escape ")").Count

        a = b

    let checkForCorrectBrackets (code: string) =
        let mutable i = 0

        while i < code.Length do

            if code[i] = '(' then
                let mutable bracketCounter = 1

                i <- i + 1

                while bracketCounter > 0 && i < code.Length do
                    if code[i] = '(' then
                        bracketCounter <- bracketCounter + 1

                    if code[i] = ')' then
                        bracketCounter <- bracketCounter - 1

                    i <- i + 1

                let flag = checkNum code

                if bracketCounter <> 0 || not flag then
                    raise (Err "Error: Incorrect brackets")
            else
                i <- i + 1

        ()
