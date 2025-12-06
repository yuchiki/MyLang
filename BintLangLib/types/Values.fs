module Values

open Ast

type environment = Map<string, value>

and value =
    | Leaf
    | Branch of value * value
    | Function of environment * string * Ast.expr
