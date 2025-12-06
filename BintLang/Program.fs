open System

let rec prettyPrint: Values.value -> string =
    function
    | Values.Leaf -> "@"
    | Values.Branch(l, r) -> $"({prettyPrint l}, {prettyPrint r})"
    | Values.Function(_, _, _) -> $"<FUN>"

type ResultBuilder() =
    member this.Bind(computation: Result<'a, 'err>, binder: 'a -> Result<'b, 'err>) : Result<'b, 'err> =
        match computation with
        | Ok a -> binder a
        | Error err -> Error err

    member this.Return(x: 'a) : Result<'a, 'err> = Ok x

    member this.Zero() = Ok()


let result = new ResultBuilder()

let handleResult: Result<unit, 'a> -> unit =
    function
    | Ok() -> ()
    | Error err ->

        raise (Exception(sprintf "error: %A" err))

let repl () =
    printfn "quit with ':q'"

    while true do
        printf "> "

        let input = stdin.ReadLine()

        if input = ":q" then
            exit 0

        let res =
            result {
                let! tokens = Tokenizer.matchString input
                let! ast = Parser.Parse tokens
                let! value = Executor.eval Map.empty ast
                return prettyPrint value
            }

        match res with
        | Ok v -> printfn "%s" v
        | Error err -> Console.WriteLine err

let oneShot () =
    result {
        let input = stdin.ReadToEnd()
        let! tokens = Tokenizer.matchString input
        let! ast = Parser.Parse tokens
        let! value = Executor.eval Map.empty ast
        prettyPrint value |> printfn "%s"
    }
    |> handleResult

[<EntryPoint>]
let main_ _ =
    if Console.IsInputRedirected then oneShot () else repl ()

    0
