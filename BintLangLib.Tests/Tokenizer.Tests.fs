module TokenizerTests

open Xunit
open Tokens
open TestUtils

let matchMustSucceedAs (expected: token list) : string -> unit =
    Tokenizer.matchString >> mustSucceedAs expected

let matchMustFail: string -> unit = Tokenizer.matchString >> mustFail

[<Fact>]
let ``Tokenizer.matchString returns empty for empty string`` () = matchMustSucceedAs [] ""

[<Fact>]
let ``Tokenizer.matchString tokenizes simple Tokens`` () =
    matchMustSucceedAs [ Leaf; LParen; RParen; Comma; Equal; Let; In ] "@(),=let in"

[<Fact>]
let ``Tokenizer.matchString tokenizes identifier`` () =
    matchMustSucceedAs [ Identifier "fooFoo"; Comma ] "fooFoo,"

[<Fact>]
let ``Tokenizer.matchString tokenizes ignoring spaces`` () =
    matchMustSucceedAs [ Leaf; LParen; RParen; Comma ] "@(  ),"

[<Fact>]
let ``Tokenizer.matchString fails for invalid string`` () = matchMustFail "abc$%"
