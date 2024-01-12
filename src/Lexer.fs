namespace Lexer

type Token =
    | LPar
    | RPar
    | Number
    | Var
    | Plus
    | Minus

module Tokenizer =
    let Tokenize (code: string list) =
        List.map
            (fun l ->
                match l with
                | "(" -> (LPar, l)
                | ")" -> (RPar, l)
                | "+" -> (Plus, l)
                | "-" -> (Minus, l)
                | _ -> (Var, l))
            code
