module Tokens

type token =
    | Leaf
    | LParen
    | RParen
    | Comma
    | Let
    | Equal
    | In
    | Identifier of string
