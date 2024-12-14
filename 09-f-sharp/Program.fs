open System
open System.IO
open System.Collections.Generic

let readFile (filename: string) =
    let frequence = Dictionary<char, int>()
    use file = new StreamReader(filename)
    let mutable buffer = ""
    while not (file.EndOfStream) do
        buffer <- file.ReadLine()
        for c in buffer do
            if Char.IsControl(c) |> not then
                if frequence.ContainsKey(c) then
                    frequence.[c] <- frequence.[c] + 1
                else
                    frequence.Add(c, 1)
    frequence

let printHistogram (frequence: Dictionary<char, int>) height barWidth =
    let sortedFrequence =
        frequence
        |> Seq.sortBy (fun kv -> kv.Key) // Сортировка по ключам
        |> Seq.toList

    let maxCount = sortedFrequence |> List.maxBy (fun kv -> kv.Value) |> fun kv -> kv.Value
    let step = float maxCount / float height

    for y in [float maxCount .. -step .. step] do
        let line =
            sortedFrequence |> List.map (fun kv ->
                if float kv.Value >= y then
                    String.replicate barWidth "#"
                else
                    String.replicate barWidth " ")
            |> String.concat ""
        printfn "%s" line

    sortedFrequence |> List.iter (fun kv -> printf "%s" (String.replicate barWidth "#"))
    printfn ""
    sortedFrequence |> List.iter (fun kv -> printf "%c%s" kv.Key (String.replicate (barWidth - 1) " "))
    printfn ""

[<EntryPoint>]
let main argv =
    match argv with
    | [| filename; heightStr; widthStr |] ->
        try
            let height = int heightStr
            let width = int widthStr
            let frequence = readFile filename
            printHistogram frequence height width
        with
        | :? FormatException ->
            printfn "Error: Column height and width must be integers"
        | ex ->
            printfn "Error: %s" ex.Message
    | _ ->
        printfn "Using: dotnet run <filename> <height> <width>"
    0
