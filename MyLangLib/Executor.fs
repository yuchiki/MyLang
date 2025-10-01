module Executor

open Ast

let rec eval: expr -> int =
    function
    | Number n -> n
    | Add(e1, e2) -> eval e1 + eval e2
    | Sub(e1, e2) -> eval e1 - eval e2
    | Mul(e1, e2) -> eval e1 * eval e2
    | Div(e1, e2) -> eval e1 / eval e2
    | Equal(e1, e2) -> if eval e1 = eval e2 then 1 else 0
