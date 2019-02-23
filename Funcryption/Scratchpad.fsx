open System
open System.Collections.Generic

let chunk chunkSize (arr : _ array) = 
    query {
        for idx in 0..(arr.Length - 1) do
        groupBy (idx / chunkSize) into g
        select (g |> Seq.map (fun idx -> arr.[idx]))
    }
    
let map items (mapping:IDictionary<string, string>) = 
    let surrogate = items 
                    |> Seq.map Char.IsSurrogate 
                    |> Seq.filter (fun x -> x = true)
                    |> Seq.length
    let count = items |> Seq.length
    match surrogate = count with 
    | true -> 
        let found, value = String.Concat items |> mapping.TryGetValue
        if found then value 
        else ""
    | false -> 
        items 
        |> Seq.map (fun x -> 
            match mapping.TryGetValue (string x) with 
            | true, value -> value
            | _ -> ""
            )
        |> String.Concat

let fromEmoji (str:string) (mapping:IDictionary<string, string>) =
    str 
    |> Seq.chunkBySize 2
    |> Seq.map (fun x -> map x mapping)
    |> String.Concat