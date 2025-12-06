module IntegrationTests

open Xunit
open TestingUtils


[<Theory>]
[<InlineData("@", "@")>]
[<InlineData("(@, @)", "(@, @)")>]
[<InlineData("(@, ((@, @), @))", "(@, ((@, @), @))")>]
[<InlineData("(   @ ,  @)", "(@, @)")>]
[<InlineData("let foo = (@, @) in (foo, foo) ", "((@, @), (@, @))")>]
[<InlineData("fun x -> (@, x)", "<FUN>")>]
[<InlineData("(fun x -> (@, x)) (@, @)", "(@, (@, @))")>]
[<InlineData("(fun y -> (fun x -> (y, x))) @ (@, @)", "(@, (@, @))")>]
let mustSucceed (input: string, output: string) =
    run input [] |> ensureSucceed |> outputIs $"{output}\n" |> ignore


[<Theory>]
[<InlineData("()")>]
[<InlineData("@ @")>]
let mustFailWithParseError (input: string) = run input [] |> ensureFail
