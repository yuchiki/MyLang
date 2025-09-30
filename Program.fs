open System
open Tokenizer
open Parser


[<EntryPoint>]
let main _ =
    while true do
        printf "> "
        let input = Console.ReadLine()
        let tokens = munch input

        match tokens with
        | Ok tokens -> printfn "Tokens: %A" tokens
        | Error e -> printfn "Error: %A" e

        let ast = Result.bind Parse tokens

        match ast with
        | Ok ast -> printfn "AST: %A" ast
        | Error e -> printfn "Error: %A" e

        let result = Result.bind (Executor.eval >> Ok) ast

        match result with
        | Ok result -> printfn "Result: %A" result
        | Error e -> printfn "Error: %A" e

    failwith "Unreachable"
