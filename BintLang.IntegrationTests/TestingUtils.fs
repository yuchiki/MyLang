module TestingUtils

open System.Diagnostics

open System
open Xunit

type ExecutionResult =
    { output: string
      error: string
      exitCode: int }


let ensureSucceed (r: ExecutionResult) : ExecutionResult =
    Assert.True(r.exitCode = 0)
    r

let ensureFail (r: ExecutionResult) : ExecutionResult =
    Assert.False(r.exitCode = 0)
    r


let outputIs (expected: string) (r: ExecutionResult) : ExecutionResult =
    Assert.Equal(expected, r.output)
    r

let solutionPath = "../../../.."

let run (input: string) (args: list<string>) : ExecutionResult =
    use executor = new Process()

    executor.StartInfo <-
        ProcessStartInfo(
            FileName = "dotnet",
            Arguments = "run --project BintLang -- " + String.Join(" ", args),
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            WorkingDirectory = solutionPath
        )


    executor.Start() |> ignore
    executor.StandardInput.Write input
    executor.StandardInput.Close()
    executor.WaitForExit()


    { output = executor.StandardOutput.ReadToEnd()
      error = executor.StandardError.ReadToEnd()
      exitCode = executor.ExitCode }
