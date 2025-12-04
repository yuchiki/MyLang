module Parser

open Tokens
open Ast
open Utils

#nowarn "40"

exception ParseError of rest: token list

type parser<'a> = token list -> Result<'a * token list, exn>


type ParserBuilder() =
    member this.Bind(parser1: parser<'a>, binder: 'a -> parser<'b>) : parser<'b> =
        fun tokens ->
            result {
                let! result, rest = parser1 tokens
                return! binder result rest
            }

    member this.Return(x: 'a) : parser<'a> = fun tokens -> Ok(x, tokens)
    member this.ReturnFrom(x: parser<'a>) : parser<'a> = x

    member this.Zero() : parser<'a> = fun tokens -> Error(ParseError tokens)

let parser = new ParserBuilder()

type ParserAlt() =
    member this.Combine(p1: parser<'a>, p2: parser<'a>) : parser<'a> =
        fun input ->
            match p1 input with
            | Ok res -> Ok res
            | Error _ -> p2 input

    member this.Zero() : parser<'a> = fun tokens -> Error(ParseError tokens)

let parserAlt = new ParserAlt()

let Consume (token: token) : parser<unit> =
    function
    | head :: rest when head = token -> Ok((), rest)
    | tokens -> Error(ParseError tokens)

let consumeIdentifier: parser<string> =
    function
    | Tokens.Identifier identifier :: rest -> Ok(identifier, rest)
    | tokens -> Error(ParseError tokens)

let rec ParsePrimary: parser<expr> =
    parserAlt {
        parser {
            do! Consume Tokens.Leaf
            return Leaf
        }

        parser {
            let! identifier = consumeIdentifier
            return Variable identifier
        }

        parser {
            do! Consume LParen
            let! lhs = ParsePrimary
            do! Consume RParen
            return lhs
        }

        parser {
            do! Consume LParen
            let! lhs = ParsePrimary
            do! Consume Comma
            let! rhs = ParsePrimary
            do! Consume RParen
            return Branch(lhs, rhs)
        }

        parser {
            do! Consume Tokens.Let
            let! identifier = consumeIdentifier
            do! Consume Tokens.Equal
            let! body = ParsePrimary
            do! Consume Tokens.In
            let! successor = ParsePrimary
            return VariableDefinition(identifier, body, successor)
        }

        parser {
            do! Consume Tokens.Fun
            let! identifier = consumeIdentifier
            do! Consume Tokens.Arrow
            let! body = ParsePrimary
            return Function(identifier, body)
        }
    }



let Parse (input: token list) : Result<expr, exn> =
    try
        match ParsePrimary input with
        | Ok(expr, rest) -> if rest.IsEmpty then Ok expr else Error(ParseError rest)
        | Error e -> Error e
    with ParseError rest ->
        Error(ParseError rest)
