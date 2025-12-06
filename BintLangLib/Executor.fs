module Executor

open Ast
open Values





type ResultBuilder() =
    member _.Bind(x, f) = Result.bind f x
    member _.Return x = Ok x
    member _.ReturnFrom x = x
    member _.Zero() = Error()

    member _.Combine(a, b) =
        match a with
        | Ok _ -> b
        | Error e -> Error e

    member _.Delay f = f ()

let result = ResultBuilder()

exception VariableNotFound of string
exception NotAFunction

let rec eval (env: environment) (e: expr) : Result<value, exn> =
    match e with
    | Ast.Leaf -> result { return Leaf }
    | Ast.Branch(lhs, rhs) ->
        result {
            let! l = eval env lhs
            let! r = eval env rhs
            return Branch(l, r)
        }
    | Ast.VariableDefinition(id, body, successor) ->
        result {
            let! v = eval env body
            let env' = env.Add(id, v)
            return! eval env' successor
        }
    | Ast.Variable id ->
        result {
            match env.TryFind id with
            | Some v -> return v
            | None -> return! Error(VariableNotFound id)
        }
    | Ast.Function(identifier, body) -> result { return Function(env, identifier, body) }
    | Ast.Application(e1, e2) ->
        result {
            let! v1 = eval env e1
            let! v2 = eval env e2

            match v1 with
            | Function(inner_env, arg, body) -> return! eval (inner_env.Add(arg, v2)) body
            | _ -> return! Error NotAFunction
        }
