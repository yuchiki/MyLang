module Ast

type expr =
    | Leaf
    | Branch of expr * expr
    | Variable of string
    | VariableDefinition of string * expr * expr
