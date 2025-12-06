module Executor

open Ast
open Values


exception VariableNotFound of string
exception NotAFunction

let rec eval' (env: environment) : expr -> value =
    function
    | Ast.Leaf -> Leaf
    | Ast.Branch(lhs, rhs) -> Branch(eval' env lhs, eval' env rhs)
    | Ast.VariableDefinition(id, body, successor) ->
        let env' = env.Add(id, eval' env body)
        eval' env' successor
    | Ast.Variable id ->
        try
            env[id]
        with _ ->
            VariableNotFound id |> raise
    | Ast.Function(identifier, body) -> Function(env, identifier, body)
    | Ast.Application(e1, e2) ->
        let v1 = eval' env e1
        let v2 = eval' env e2

        match v1 with
        | Function(inner_env, arg, body) -> eval' (inner_env.Add(arg, v2)) body
        | _ -> raise NotAFunction

let eval (env: environment) (e: expr) : Result<value, exn> =
    try
        e |> eval' env |> Ok
    with VariableNotFound _ as e ->
        Error e
