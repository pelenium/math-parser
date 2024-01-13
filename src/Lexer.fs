namespace Lexer

exception Err of string

type Token =
    | LPar
    | RPar
    | Number
    | Plus
    | Minus

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
