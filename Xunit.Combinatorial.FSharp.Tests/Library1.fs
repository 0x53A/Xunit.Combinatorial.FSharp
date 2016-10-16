namespace Xunit.Combinatorial.FSharp.Tests

open Xunit

type TestRecord = { A : int ; B : bool }

module CombinatorialFSharpParameterTests =

    [<CombinatorialData>]
    [<Theory>]
    let A( [<Xunit.Combinatorial.CombinatorialFSharpParameter>] x : TestRecord) =
        ()
