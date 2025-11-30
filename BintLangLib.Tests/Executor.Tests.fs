module Executor.Tests

open Xunit
open TestUtils
open Ast

let evalMustSucceedAs (expected: Values.value) : expr -> unit =
    eval Map.empty >> mustSucceedAs expected

let evalWithEnvMustSucceedAs (env: environment) (expected: Values.value) : expr -> unit =
    eval env >> mustSucceedAs expected

let evalMustFail: expr -> unit = eval Map.empty >> mustFail

[<Fact>]
let ``base cases`` () =
    Leaf |> evalMustSucceedAs Values.Leaf

    Branch(Ast.Leaf, Ast.Leaf)
    |> evalMustSucceedAs (Values.Branch(Values.Leaf, Values.Leaf))

    Variable "foo"
    |> evalWithEnvMustSucceedAs (Map.ofList [ ("foo", Values.Leaf) ]) Values.Leaf

    Variable "notExistingVar" |> evalMustFail

    VariableDefinition("foo", Leaf, Branch(Variable "foo", Variable "foo"))
    |> evalMustSucceedAs (Values.Branch(Values.Leaf, Values.Leaf))

    Function("x", Leaf) |> evalMustSucceedAs (Values.Function("x", Leaf))
