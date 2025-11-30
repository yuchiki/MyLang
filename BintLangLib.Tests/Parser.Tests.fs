module Parser.Tests

open Xunit
open Tokens
open TestUtils

let parseMustSucceedAs (expected: Ast.expr) : token list -> unit = Parse >> mustSucceedAs expected

let parseMustFail: token list -> unit = Parse >> mustFail

[<Fact>]
let ``parse Leaf`` () = [ Leaf ] |> parseMustSucceedAs Ast.Leaf

[<Fact>]
let ``parse Branch`` () =
    [ LParen; Leaf; Comma; Leaf; RParen ]
    |> parseMustSucceedAs (Ast.Branch(Ast.Leaf, Ast.Leaf))


[<Fact>]
let ``parse simple paren`` () =
    [ LParen; Leaf; RParen ] |> parseMustSucceedAs Ast.Leaf

[<Fact>]
let ``parse variable definition`` () =
    [ Let; Identifier "xxx"; Equal; Leaf; In; Identifier "zzz" ]
    |> parseMustSucceedAs (Ast.VariableDefinition("xxx", Ast.Leaf, Ast.Variable "zzz"))

[<Fact>]
let ``parse variable`` () =
    [ Identifier "foo" ] |> parseMustSucceedAs (Ast.Variable "foo")

[<Fact>]
let ``parse function`` () =
    [ Fun; Identifier "x"; Arrow; Identifier "y" ]
    |> parseMustSucceedAs (Ast.Function("x", Ast.Variable "y"))


[<Fact>]
let ``parse Branch recursively`` () =
    [ LParen
      LParen
      Leaf
      Comma
      LParen
      Leaf
      Comma
      Leaf
      RParen
      RParen
      Comma
      Leaf
      RParen ]
    |> parseMustSucceedAs (Ast.Branch(Ast.Branch(Ast.Leaf, Ast.Branch(Ast.Leaf, Ast.Leaf)), Ast.Leaf))

[<Fact>]
let ``parse fails if unparsable`` () = [ Leaf; Leaf ] |> parseMustFail
