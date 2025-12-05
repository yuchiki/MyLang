open System
open Utils

let rec prettyPrint: Values.value -> string =
    function
    | Values.Leaf -> "@"
    | Values.Branch(l, r) -> $"({prettyPrint l}, {prettyPrint r})"
    | Values.Function(x, body) -> $"<FUN>"


let handleResult: Result<unit, 'a> -> unit =
    function
    | Ok() -> ()
    | Error err ->

        eprintfn "error: %A" err
        raise (Exception(sprintf "error: %A" err))

[<EntryPoint>]
let main_ _ =
    result {
        let input = stdin.ReadToEnd().Trim()

        let! tokens =
            match Tokenizer.matchString input with
            | Ok t -> Ok t
            | Error e ->
                eprintfn "Tokenizer error: %A" e
                Error e

        let! ast =
            match Parser.Parse tokens with
            | Ok a -> Ok a
            | Error e ->
                eprintfn "Parser error: %A" e
                Error e

        let! value =
            match Executor.eval Map.empty ast with
            | Ok v -> Ok v
            | Error e ->
                eprintfn "Executor error: %A" e
                Error e

        prettyPrint value |> printfn "%s"
    }
    |> handleResult

    0
