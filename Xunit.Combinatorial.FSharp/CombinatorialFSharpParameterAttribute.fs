namespace Xunit.Combinatorial

open System
open FSharp.Reflection

[<AttributeUsage(AttributeTargets.Parameter)>]
type CombinatorialFSharpParameterAttribute() =
    inherit Attribute()

    let allPermutations values =
        let rec allPermutations_r (values: obj array array) n : obj list seq = seq {
            if values.Length = 0 then
                yield! [| |]
            else if n = values.Length - 1 then
                for v in values.[n] do
                    yield [ v ]
            else 
                let subPermutations = allPermutations_r values (n+1)
                for v in values.[n] do
                    for x in subPermutations do
                        yield v :: x
        }
        allPermutations_r values 0

    let rec tryProvideFor (typ:Type) : obj array option =
        if typ = typeof<bool> then
            Some <| [| true ; false |]
        else if typ = typeof<int> then
            Some <| [| 0 ; 1 |]
        else if typ.IsEnum then
            Some <| [| for n in Enum.GetNames typ -> Enum.Parse(typ, n) |]
        else if FSharpType.IsRecord typ then
            let fields = FSharpType.GetRecordFields typ
            let fieldTypes = fields |> Array.map (fun p -> p.PropertyType)
            let fieldValuesOpt = fieldTypes |> Array.map tryProvideFor
            if fieldValuesOpt |> Seq.exists (fun f -> f.IsNone) then
                None
            else
                let ctor = typ.GetConstructor fieldTypes
                let fieldValues = fieldValuesOpt |> Array.map Option.get
                let permutations = allPermutations fieldValues |> Seq.map Seq.toArray |> Seq.toArray
                Some (permutations |> Array.map ctor.Invoke)
//        else if FSharpType.IsTuple typ then
//            Some ...
//        else if FSharpType.IsUnion typ then
//            Some ...
        else
            None

    interface Xunit.ICombinatorialValuesProvider with
        member x.GetValues(p) =
            let typ = p.ParameterType
            match tryProvideFor typ with
            | Some v -> v
            | None -> raise <| InvalidOperationException (sprintf "Can't provide values for type %O" typ)
