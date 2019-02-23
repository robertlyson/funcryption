namespace Funcryption

open System
open System.Collections.Generic

module Logic = 

    let mapping = dict[
        ' ', " ";
        'a', "\ud83d\ude00";
        'b', "\U0001F602";
        'c', "\U0001F603";
        'd', "\U0001F604";
        'e', "\U0001F605";
        'f', "\U0001F606";
        'g', "\U0001F607";
        'h', "\U0001F608";
        'i', "\U0001F609";
        'j', "\U0001F60A";
        'k', "\U0001F60B";
        'l', "\U0001F60C";
        'm', "\U0001F60D";
        'n', "\U0001F60F";
        'o', "\U0001F612";
        'p', "\U0001F613";
        'q', "\U0001F614";
        'r', "\U0001F616";
        's', "\U0001F618";
        't', "\U0001F61A";
        'u', "\U0001F61C";
        'v', "\U0001F61D";
        'w', "\U0001F61E";
        'x', "\U0001F620";
        'y', "\U0001F621";
        'z', "\U0001F622";

        'A', "\U0001F40C";
        'B', "\U0001F40D";
        'C', "\U0001F40E";
        'D', "\U0001F411";
        'E', "\U0001F412";
        'F', "\U0001F414";
        'G', "\U0001F417";
        'H', "\U0001F418";
        'I', "\U0001F419";
        'J', "\U0001F41A";
        'K', "\U0001F41B";
        'L', "\U0001F41C";
        'M', "\U0001F41D";
        'N', "\U0001F41E";
        'O', "\U0001F41F";
        'P', "\U0001F420";
        'Q', "\U0001F421";
        'R', "\U0001F422";
        'S', "\U0001F423";
        'T', "\U0001F424";
        'U', "\U0001F425";
        'V', "\U0001F426";
        'W', "\U0001F427";
        'X', "\U0001F428";
        'Y', "\U0001F429";
        'Z', "\U0001F42B";

        '0', "\U00012648";
        '1', "\U00012649";
        '2', "\U0001264A";
        '3', "\U0001264B";
        '4', "\U0001264C";
        '5', "\U0001264D";
        '6', "\U0001264E";
        '7', "\U0001264F";
        '8', "\U00012650";
        '9', "\U00012651";
    ]

    let toEmoji (list:seq<char>) =
        list
        |> Seq.map (fun x ->  mapping.TryGetValue x)
        |> Seq.map (fun (x,y) -> if x then y else "_")
        
    let tryFindKeyByValue (dic:IDictionary<char, string>) (value:string) =
         let value = dic |> Seq.tryPick (fun x -> if x.Value = value then Some(x.Key) else None)
         match value with
         | Some x -> x
         | None -> ' '

    let map items (mapping:IDictionary<char, string>) = 
        let surrogate = items 
                        |> Seq.map Char.IsSurrogate 
                        |> Seq.filter (fun x -> x = true)
                        |> Seq.length
        let count = items |> Seq.length
        match surrogate = count with 
        | true -> 
            tryFindKeyByValue mapping (String.Concat items) |> string
        | false -> 
            items 
            |> Seq.map (fun x -> tryFindKeyByValue mapping (string x) )
            |> String.Concat

    let fromEmoji (str:string) =
        str 
        |> Seq.chunkBySize 2
        |> Seq.map (fun x -> map x mapping)
        |> String.Concat

    let encode input = 
        input |> toEmoji |> String.Concat

    let decode input = 
        input
        |> fromEmoji