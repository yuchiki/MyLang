module Executor

open Ast
open Values


exception VariableNotFound of exn

type environment = Map<string, value>

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
        with e ->
            e |> VariableNotFound |> raise

let eval (env: environment) (e: expr) : Result<value, exn> =
    try
        e |> eval' env |> Ok
    with VariableNotFound _ as e ->
        Error e
