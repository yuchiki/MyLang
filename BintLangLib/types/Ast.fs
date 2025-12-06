module Ast


type expr =
    | Leaf
    | Branch of expr * expr
    | Variable of string
    | VariableDefinition of string * expr * expr
    | Function of string * expr
    | Application of expr * expr
