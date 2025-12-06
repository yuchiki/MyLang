module Executor

open Ast
open Values


exception VariableNotFound of string
exception NotAFunction

let rec eval' (env: environment) : expr -> Result<value, exn> =
    function
    | Ast.Leaf -> Ok Leaf
    | Ast.Branch(lhs, rhs) ->
        match eval' env lhs, eval' env rhs with
        | Ok l, Ok r -> Ok(Branch(l, r))
        | Error e, _ -> Error e
        | _, Error e -> Error e
    | Ast.VariableDefinition(id, body, successor) ->
        match eval' env body with
        | Ok v ->
            let env' = env.Add(id, v)
            eval' env' successor
        | Error e -> Error e
    | Ast.Variable id ->
        match env.TryFind(id) with
        | Some v -> Ok v
        | None -> Error(VariableNotFound id)
    | Ast.Function(identifier, body) -> Ok(Function(env, identifier, body))
    | Ast.Application(e1, e2) ->
        match eval' env e1, eval' env e2 with
        | Ok(Function(inner_env, arg, body)), Ok v2 -> eval' (inner_env.Add(arg, v2)) body
        | Ok _, Ok _ -> Error NotAFunction
        | Error e, _ -> Error e
        | _, Error e -> Error e

let eval (env: environment) (e: expr) : Result<value, exn> = eval' env e
