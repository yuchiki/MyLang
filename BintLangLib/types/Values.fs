module Values

type value =
    | Leaf
    | Branch of value * value
    | Function of string * Ast.expr
