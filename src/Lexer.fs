namespace Lexer

exception Err of string

type Token =
    | LPar
    | RPar
    | Number
    | Plus
    | Minus

module StackMachine =

    type StackMachine() =
        let mutable Stack: int list = []

        member _.PUSH(n: int) = Stack <- Stack @ [ n ]

        member _.POP() =
            let head = Stack.Head
            Stack <- Stack.Tail

            head

        member this.ADD() =
            if Stack.Length < 2 then
                failwith "Incorrect opcode"

            Stack <- (this.POP() + this.POP()) :: Stack

        member this.SUB() =
            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            Stack <- (this.POP() - this.POP()) :: Stack

        member this.MUL() =
            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            Stack <- (this.POP() * this.POP()) :: Stack

        member this.DIV() =
            // this.Print()

            if Stack.Length < 2 then
                raise (Err "Error: Incorrect opcode")

            let fst = this.POP()
            let scnd = this.POP()

            if scnd = 0 then
                raise (Err "Error: Division by zero")

            Stack <- (fst / scnd) :: Stack

        member _.Print() =
            for i in Stack do
                printf $"{i} "

            printfn ""

module private OperationStack =
    type OperationStack() =
        let mutable Stack: string list = []

        member _.PUSH(n: string) = Stack <- Stack @ [ n ]

        member _.POP() =
            let head = Stack.Head
            Stack <- Stack.Tail

            head

module Tokenizer =
    type Token =
        | LPar
        | RPar
        | Number
        | Plus
        | Minus
        | Identidier

    let private ToInt (token: string) =
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

    let parse (code: string) =
        let os = OperationStack.OperationStack()
        let tokens = code |> split |> Tokenize

        for token in tokens do
            printfn $"{token}"

            match token with
            | (Plus, _) -> os.PUSH "+"
            | (Minus, _) -> os.PUSH "-"
            | (LPar, _) -> ()
            | _ -> ()

        ()

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
