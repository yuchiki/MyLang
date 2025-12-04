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

        raise (Exception(sprintf "error: %A" err))

[<EntryPoint>]
let main_ _ =
    result {
        let input = stdin.ReadToEnd()
        let! tokens = Tokenizer.matchString input
        let! ast = Parser.Parse tokens
        let! value = Executor.eval Map.empty ast
        prettyPrint value |> printfn "%s"
    }
    |> handleResult

    0
