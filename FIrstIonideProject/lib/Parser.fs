module Parser

open Tokens
open Ast

#nowarn "40"

exception ParseError of rest: token list

type parser = token list -> expr * token list

let preConsume (expected: token) (p2: parser) : parser =
    function
    | t :: rest when t = expected -> p2 rest
    | rest -> raise (ParseError rest)

let postConsume (expected: token) (p2: parser) : parser =
    fun input ->
        let expr, rest = p2 input

        match rest with
        | t :: rest' when t = expected -> expr, rest'
        | rest' -> raise (ParseError rest')

let rec applyAsMany (f: 'Acc -> 'State -> 'Acc option * 'State) (initAcc: 'Acc) (initState: 'State) : 'Acc * 'State =
    match f initAcc initState with
    | Some acc, state -> applyAsMany f acc state
    | None, state -> initAcc, state

let foldBinaryOperator
    (operatorMap: Map<token, expr * expr -> expr>)
    (nextParser: parser)
    (input: token list)
    : expr * token list =
    let foldBinaryOperator'
        (operatorMap: Map<token, expr * expr -> expr>)
        (nextParser: parser)
        (acc: expr)
        (input: token list)
        : expr option * token list =
        match input with
        | op :: rest when operatorMap.ContainsKey op ->
            let nextExpr, rest' = nextParser rest

            Some((operatorMap[op]) (acc, nextExpr)), rest'
        | _ -> None, input

    let firstExpr, rest = nextParser input
    applyAsMany (foldBinaryOperator' operatorMap nextParser) firstExpr rest

let rec ParseAdditive: parser =
    foldBinaryOperator (Map [ Plus, Add; Minus, Sub ]) ParseMultiplicative

and ParseMultiplicative: parser =
    foldBinaryOperator (Map [ Asterisk, Mul; Slash, Div ]) ParsePrimary

and ParsePrimary: parser =
    function
    | Tokens.Number n :: rest -> Number n, rest
    | LParen :: _ as input -> input |> (preConsume LParen <| ParseAdditive |> postConsume RParen)
    | rest -> raise (ParseError rest)

let Parse (input: token list) : Result<expr, exn> =
    try
        let expr, rest = ParseAdditive input
        if rest.IsEmpty then Ok expr else Error(ParseError rest)
    with ParseError rest ->
        Error(ParseError rest)
