module Tokenizer

open System
open Tokens

exception MunchError of rest: string

let tokenMap =
    Map
        [ "+", Plus
          "-", Minus
          "*", Asterisk
          "/", Slash
          "(", LParen
          ")", RParen
          "==", Equal ]

let spaceLike: char list = [ ' '; '\t'; '\n'; '\r' ]

let (|Empty|Cons|) (s: string) =
    if s = "" then Empty else Cons(s[0], s[1..])

let (|MunchToken|_|) (input: string) : (token * string) option =
    let matchedToken =
        tokenMap.Keys |> Seq.tryFind (fun token -> input.StartsWith token)

    match matchedToken with
    | Some op -> Some(tokenMap[op], input[op.Length ..])
    | None -> None

let (|MunchNumber|_|) (input: string) : (token * string) option =
    let (|Digit|NonDigit|) (c: char) =
        if Char.IsDigit c then
            Digit(c |> string |> int)
        else
            NonDigit c

    let rec munchDigits: string -> int list * string =
        function
        | Empty -> [], ""
        | Cons(NonDigit _, _) as input -> [], input
        | Cons(Digit d, rest) ->
            let ds, rest' = munchDigits rest
            d :: ds, rest'

    match munchDigits input with
    | [], _ -> None
    | ds, rest ->
        ds
        |> List.fold (fun acc d -> acc * 10 + d) 0
        |> Number
        |> fun n -> Some(n, rest)

let (|MunchSpace|_|) =
    function
    | Cons(c, rest) when List.contains c spaceLike -> Some rest
    | _ -> None

let rec munch1: string -> (token option * string) option =
    function
    | Empty -> None
    | MunchSpace rest -> Some(None, rest)
    | MunchToken(token, rest) -> Some(Some token, rest)
    | MunchNumber(token, rest) -> Some(Some token, rest)
    | rest -> raise (MunchError rest)

let munch (input: string) : Result<token list, exn> =
    try
        input
        |> List.unfold munch1
        |> List.filter Option.isSome
        |> List.map Option.get
        |> Ok
    with MunchError rest ->
        Error(MunchError rest)
