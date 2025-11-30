module Tokens

type token =
    | Leaf
    | LParen
    | RParen
    | Comma
    | Let
    | Equal
    | In
    | Fun
    | Arrow
    | Identifier of string
