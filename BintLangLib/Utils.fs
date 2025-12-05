module Utils

type ResultBuilder() =
    member this.Bind(computation: Result<'a, 'err>, binder: 'a -> Result<'b, 'err>) : Result<'b, 'err> =
        match computation with
        | Ok a -> binder a
        | Error err -> Error err

    member this.Return(x: 'a) : Result<'a, 'err> = Ok x

    member this.ReturnFrom(res: Result<'a, 'err>) : Result<'a, 'err> = res

    member this.Zero() = Ok()


let result = new ResultBuilder()
