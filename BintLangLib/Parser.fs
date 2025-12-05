module Parser

open Tokens
open Ast
open Utils

#nowarn "40"

exception ParseError of rest: token list

type parser<'a> = token list -> Result<'a * token list, exn>


let Consume (token: token) : parser<unit> =
    function
    | head :: rest when head = token -> Ok((), rest)
    | tokens -> Error(ParseError tokens)

let consumeIdentifier: parser<string> =
    function
    | Tokens.Identifier identifier :: rest -> Ok(identifier, rest)
    | tokens -> Error(ParseError tokens)



type ParserBuilder() =
    member this.Bind(parser1: parser<'a>, binder: 'a -> parser<'b>) : parser<'b> =
        fun tokens ->
            result {
                let! result, rest = parser1 tokens
                return! binder result rest
            }

    member this.Return(x: 'a) : parser<'a> = fun tokens -> Ok(x, tokens)
    member this.ReturnFrom(x: parser<'a>) : parser<'a> = x

    member this.Combine(consumer: parser<unit>, successor: parser<'a>) : parser<'a> =
        fun tokens ->
            match consumer tokens with
            | Error err -> Error err
            | Ok((), rest) -> successor rest

    member this.Zero() : parser<'a> = fun tokens -> Error(ParseError tokens)

let parser = new ParserBuilder()

type ParserAltBuilder() =
    member _.Combine(p1: parser<'a>, p2: parser<'a>) : parser<'a> =
        fun input ->
            match p1 input with
            | Ok res -> Ok res
            | Error _ -> p2 input

    member _.ReturnFrom(p: parser<'a>) : parser<'a> = p
    member _.Delay(f: unit -> parser<'a>) : parser<'a> = fun tokens -> f () tokens
    member _.Zero() : parser<'a> = fun tokens -> Error(ParseError tokens)

let parserAlt = new ParserAltBuilder()

let rec ParseExpr: parser<expr> =
    parserAlt {
        return!
            parser {
                do! Consume LParen
                let! expr = ParseExpr
                do! Consume RParen
                return expr
            }

        return!
            parser {
                do! Consume LParen
                let! lhs = ParseExpr
                do! Consume Comma
                let! rhs = ParseExpr
                do! Consume RParen

                return Branch(lhs, rhs)
            }

        return!
            parser {
                do! Consume Tokens.Leaf
                return Leaf
            }

        return!
            parser {
                let! identifier = consumeIdentifier
                return Variable identifier
            }

        return!
            parser {
                do! Consume Tokens.Let
                let! identifier = consumeIdentifier
                do! Consume Tokens.Equal
                let! body = ParseExpr
                do! Consume Tokens.In
                let! successor = ParseExpr
                return VariableDefinition(identifier, body, successor)
            }

        return!
            parser {
                do! Consume Tokens.Fun
                let! identifier = consumeIdentifier
                do! Consume Tokens.Arrow
                let! body = ParseExpr
                return Function(identifier, body)
            }
    }

let Parse: token list -> Result<expr, exn> =
    fun input ->
        try
            match ParseExpr input with
            | Ok(expr, rest) -> if rest.IsEmpty then Ok expr else Error(ParseError rest)
            | Error e -> Error e
        with ParseError rest ->
            Error(ParseError rest)
