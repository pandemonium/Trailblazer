namespace Trailblazer.Infrastructure

open System.Reflection
open Microsoft.FSharp.Reflection


module Reflect =
  let rec serialize subject : string =
    let serializeLeaf =
      string subject
    let inline serializeField (property : PropertyInfo) =
      FSharpValue.GetRecordField(subject, property)
      |> serialize
      |> sprintf "%s: %s" property.Name

    let serializeBranch =
      FSharpType.GetRecordFields
      >> Array.map serializeField
      >> Array.reduce (sprintf "%s, %s")

    let tpe = subject.GetType ()
    if FSharpType.IsRecord tpe
      then serializeBranch tpe
      else serializeLeaf

  type 'a Labeled = PropertyInfo * 'a
  let rec linearize f e subject =
    let inline tree path property =
      let value = FSharpValue.GetRecordField (subject, property)

      if FSharpType.IsRecord property.PropertyType
        then linearize f path value
        else f path <| (property, value)

    subject.GetType ()
    |> FSharpType.GetRecordFields
    |> Array.fold tree e