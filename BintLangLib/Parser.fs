module Parser

open Tokens
open Utils

exception ParseError of rest: token list

type parser<'a> = token list -> Result<'a * token list, exn>

#nowarn 40 // computation 式の中の再帰のための警告をサプレスする

let consume (token: token) : parser<unit> =
    function
    | head :: rest when head = token -> Ok((), rest)
    | tokens -> Error(ParseError tokens)

let consumeIdentifier: parser<string> =
    function
    | Identifier identifier :: rest -> Ok(identifier, rest)
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

let rec ParseExpr: parser<Ast.expr> =
    parserAlt {
        return!
            parser {
                do! consume LParen
                let! expr = ParseExpr
                do! consume RParen
                return expr
            }

        return!
            parser {
                do! consume LParen
                let! lhs = ParseExpr
                do! consume Comma
                let! rhs = ParseExpr
                do! consume RParen

                return Ast.Branch(lhs, rhs)
            }

        return!
            parser {
                do! consume Leaf
                return Ast.Leaf
            }

        return!
            parser {
                let! identifier = consumeIdentifier
                return Ast.Variable identifier
            }

        return!
            parser {
                do! consume Let
                let! identifier = consumeIdentifier
                do! consume Equal
                let! body = ParseExpr
                do! consume In
                let! successor = ParseExpr
                return Ast.VariableDefinition(identifier, body, successor)
            }

        return!
            parser {
                do! consume Fun
                let! identifier = consumeIdentifier
                do! consume Arrow
                let! body = ParseExpr
                return Ast.Function(identifier, body)
            }
    }

let Parse: token list -> Result<Ast.expr, exn> =
    fun input ->
        match ParseExpr input with
        | Ok(expr, []) -> Ok expr
        | Ok(_, rest) -> Error(ParseError rest)
        | Error e -> Error e
