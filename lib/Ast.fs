module Ast

type expr =
    | Number of int
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Equal of expr * expr
